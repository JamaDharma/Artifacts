================================================================================
ARTIFACT BUILD OPTIMIZER - AI CONTEXT
================================================================================

WHAT IT DOES:
Find optimal 5-artifact builds for Genshin Impact characters by:
- Generating artifacts with realistic RNG distributions
- Scoring via iteratively-refined stat weights
- Tracking progression (damage vs artifact count)

KEY DOMAIN CONCEPTS:
- Artifact: {piece, set, mainStat, 4 substats} with 4-5 upgrade rolls (7-10 value each)
- Build: 5 artifacts (Flower, Plume, Sands, Goblet, Circlet), often needs 4pc set bonus
- Character: {scaling stats, damage formula, optional constraints like ER>=200%}
- Optimization: Find best combination from artifact pool using weighted stat sum

ARCHITECTURE:
Two-tier design separating I/O from optimization:
1. ARTIFACT LAYER (I/O, display, testing):
   - Types: Artifact, Build, [(Stat, Double)]
   - Modules: Artifact.hs, ImportGOOD.hs, Main.hs, Core.Interface.hs
2. CORE LAYER (hot path, optimized):
   - Types: ArtifactInfo, BuildInfo, Statline, Weightline
   - Modules: Core.Utils, Core.Traversal, Core.SearchEngine, Core.Pareto, Core.Progression
   - Conversion at boundaries: Artifact <-> ArtifactInfo (via toArtifactInfo/aiOriginal)

DATA TYPES:
Core.Utils defines the high-performance types:
- Stat: HPf/ATKf/DEFf (flat), HP/ATK/DEF/ER/EM/CR/CD/HB/DMG/DMGb (%)
- Statline: Strict 9-field record {slHP, slATK, slDEF, slER, slEM, slCR, slCD, slHB, slDMG}
  * O(1) field access via INLINE accessors, no Array overhead
  * Flat stats normalized before creation (HPf -> HP%, etc)
  * Used for both stat storage and weight vectors
- Weightline: Type alias for Statline, enables O(1) dot product scoring
- ArtifactInfo: {aiPiece, aiMainStat, aiStatline, aiOriginal}
  * Pre-normalized stats (artifact contribution only, char stats added separately)
  * Caches piece/mainStat for quick access
  * aiStatline excludes flat stats (already normalized to %)
  * aiOriginal preserves Artifact for display/export
- BuildInfo: [ArtifactInfo] (core representation of a build)
- BuildComponents: [[ArtifactInfo]] (piece groups for one build variant)

CHARACTERS (Character.hs, CharacterLibrary.hs):
Character.hs: Type definition only (avoids cyclic dependencies)
  - Data Character with scaling stats, base stats, damage calculator, constraints
CharacterLibrary.hs: Character instances + ArtifactInfo conversion utilities
  - furina: HP/ER/CR/CD scaling, 200% ER constraint
  - nefer: EM/CR/CD scaling, no constraints
  - collectStatsNormalized: [(Stat,Double)] -> Statline (normalizes HPf->HP%, etc)
  - toArtifactInfo: Character -> Artifact -> ArtifactInfo (converts + normalizes)

CORE OPTIMIZATION MODULES:

Core.Utils (type definitions + conversions):
- ArtifactInfo/BuildInfo type definitions
- toArtifactInfo: Artifact -> ArtifactInfo (pre-normalize stats for hot path)
- collectStatsNormalized: Convert [(Stat,Double)] to Statline (flat->% normalization)
- rollsToWeightline: [(Stat, Double)] -> Weightline (per-roll -> per-value weights)
- defaultWeightline: Character -> Weightline (initial 1.0 per roll for each scaling stat)

Core.Traversal (build enumeration + scoring):
- traverseBuildComponents: Core fold over all 5-piece combinations
  * Accumulates Statline incrementally (avoids repeated conversions)
  * Tracks (best build, best score) + all statlines for weight calculation
- scoreArtifactInfo: Weightline -> ArtifactInfo -> Double (O(1) dot product)
- best4pcStatlines: Main search entry
  * Returns ((BuildInfo, score), [Statline]) for weight refinement
  * Enumerates on-set + off-piece variants
- bestPiecesInfo: Take top N artifacts per piece (sorted by score)
- prepareComponentSets: Generate build variant structure (on-set vs off-piece)

Core.SearchEngine (optimization loop + weight refinement):
- bestBuildInfo: MAIN ENTRY POINT for optimization
  * Iterative weight refinement until convergence
  * Returns BuildInfo (convert to Build via map aiOriginal)
  * Algorithm:
    1. Start with equal weights (1.0 per roll for each scaling stat)
    2. Convert to Weightline (per-roll -> per-value conversion)
    3. Search for best builds using current weights (best4pcStatlines)
    4. Calculate new weights via sensitivity analysis (calcStatWeightsStatlines)
    5. Repeat until damage stops improving
- calcStatWeightsStatlines: Sensitivity-based weight update
  * Buff each stat by Â±17 rolls (~2 good rolls)
  * Measure damage slope: (dmg_plus - dmg_minus) / base_dmg * 100 / 4
  * Tests against all statlines from current search (not just best build)
- calcStatWeightsCInfo: Constraint-aware weights (experimental, convergence issues)
- buildInfoToStatline: BuildInfo -> Statline (char stats + artifact stats)

Core.Pareto (dominance filtering):
- paretoFilterRealInfo: Strict Pareto frontier (fully optimal subset)
  * Artifact A dominates B if A >= B on ALL scaling stats AND A > B on at least one
  * Returns only non-dominated artifacts
- paretoFilterInfo: Forward-only filter (preserves artifacts seen first)
  * Order-dependent: early artifacts stay even if later ones dominate
  * Used for stable sorting in some contexts
- Per-piece filtering: Grouping by aiPiece, then filter each group independently

Core.Progression (artifact accumulation simulation):
- progression: Track best build at each artifact count
  * Simulates gradual artifact acquisition
  * Returns [(Int, Build)] - list of (artifact_index, best_build_at_that_point)
  * Internal: operates on ArtifactInfo, converts at boundaries
  * Uses reverse-indexed processing + pareto filtering for efficiency

Core.Interface (display/test bridge - NOT hot path):
- bestBuildNew: Wrapper around bestBuildInfo
  * Handles Artifact -> ArtifactInfo conversion at entry
  * Converts BuildInfo -> Build at exit (map aiOriginal)
  * Will likely be renamed to bestBuild
- paretoFilter/paretoFilterReal: Wrappers for Pareto functions
  * Convert Artifact -> ArtifactInfo, call Core.Pareto, convert back
- Legacy/experimental functions (unused):
  * bestBuild, bestBuildFolding (old implementations, bugs)
  * BuildStrategy infrastructure (experimental, currently unused)
  * fold4pcBuilds, best4pcBuilds (old API kept for tests)

GENERATION (Generator.hs, Weights.hs):
- Realistic probability distributions for piece/stat/upgrade selection
- 4:1 ratio for 4 vs 5 upgrades, weighted mainStat per piece type
- generateArtifactForPiece: targeted generation (used by UpgradeSimulator)

SIMULATION (Main.hs):
- Concurrent runs (10K artifacts x 10 sets) using mapConcurrentlyBounded
- Uses progression + bestBuildInfo to track damage vs artifact count
- Exports JSON for Chart.js visualization

IMPORT/EXPORT:
- ImportGOOD.hs: JSON <-> Artifact conversion (GOOD format), real build loading
  * readGOODLevelled: loads only level 20 artifacts (used by upgrade simulator)
  * WARNING: Elemental damage bonuses (Hydro/Pyro/etc DMG%) hardcoded to HP% (Furina-specific hack)
  * This affects import accuracy for characters using elemental goblets
- ExportPlot.hs: progression data -> Chart.js JSON

UPGRADE SIMULATOR (Core.Upgrades):
- simulateUpgrades: Main entry point for artifact replacement analysis
  * Loads real artifacts from GOOD format, runs N progression simulations
  * Tracks when each artifact stops appearing in optimal builds
  * Returns statistics: median replacement index, probability, rating
- Key design: Real artifacts indexed at 0, generated at 1..N for progression tracking
- Tracking: Map ArtifactInfo Int, uses custom Eq/Ord (includes set for identity)
- Statistics: aggregateStats computes median/probability across runs
  * Rating = median * probability (sort key: high = hard to replace)
  * Hidden gems: artifacts not in initial build but appear later (prob < 1.0)
- Output: Table sorted by rating (keepers first, easy replacements last)

TESTING (test/):
- Tests.hs: Regression suite, real build analysis, progression benchmarks
- GeneratorUtils.hs: Deterministic RNG (withDeterministicRandom), timing utilities
- BuildSearchComparision.hs: Depth regression tests vs recorded benchmarks (TestData.hs)
- Unit/ParetoSpec.hs: QuickCheck properties for Pareto filtering

PERFORMANCE NOTES:
- Statline: Strict record with INLINE accessors, O(1) dot product, avoids Array overhead
- ArtifactInfo: Pre-normalized stats (one conversion at entry vs repeated conversions)
- Weightline dot product: 9 multiplications vs list iteration + lookup
- Pareto filtering: Can reduce artifact count by ~50% with minimal quality loss
- Incremental Statline accumulation in traversal (vs rebuilding each time)

CURRENT ISSUES:
- calcStatWeightsCInfo (constraint-aware) has convergence issues
- bestBuildFolding has bugs, unusable
- BuildStrategy infrastructure experimental/unused

QUICK FILE GUIDE:
Artifact.hs - Core Artifact type, stat conversions, CV/RV helpers
Statline.hs - Efficient stat container with strict fields + INLINE accessors
Character.hs - Character type definition (avoids cyclic deps)
CharacterLibrary.hs - Character instances + ArtifactInfo utilities
Core.Utils - ArtifactInfo/BuildInfo types, conversions, Weightline helpers
Core.Traversal - Build enumeration, scoring, search
Core.SearchEngine - Optimization loop, weight refinement (MAIN LOGIC)
Core.Pareto - Dominance filtering
Core.Progression - Track builds over time, aggregate runs
Core.Interface - Display/test bridge (wrappers for Artifact <-> ArtifactInfo conversion)
Generator.hs - RNG artifact creation
Weights.hs - Probability distributions
Main.hs - Simulation runner
ImportGOOD.hs - JSON I/O
ExportPlot.hs - Visualization export
UpgradeSimulator.hs - Test upgrade difficulty
RollProbability.hs - Estimate artifact roll counts
Tests.hs - Test suite

================================================================================
