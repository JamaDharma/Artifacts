================================================================================
ARTIFACT BUILD OPTIMIZER - PROJECT DIGEST FOR AI ASSISTANCE
================================================================================

PROJECT OVERVIEW:
-----------------
Haskell application for optimizing artifact builds in Genshin Impact (gacha game).
Simulates artifact generation, evaluates build quality, finds optimal combinations,
and tracks progression over time.

KEY CONCEPTS:
-------------
- Artifacts: Equipment pieces with main stat + 4 substats (HP, ATK, DEF, CR, CD, ER, EM, etc.)
- Builds: Set of 5 artifacts (Flower, Plume, Sands, Goblet, Circlet)
- Character: Has scaling stats, base stats, damage calculation formula
- Optimization: Find best 5-piece combination from artifact pool using stat weights

CORE MODULES (app/):
-------------------

1. ArtifactType.hs - FUNDAMENTAL DATA TYPES
   - Piece: Flower|Plume|Goblet|Sands|Circlet
   - Stat: HPf|ATKf|DEFf|HP|ATK|DEF|ER|EM|CR|CD|HB|DMG|DMGb
   - Artifact: {piece, set, upNumber, stats}
   - Build: [Artifact] (list of 5)
   - Stat conversion functions (rolls ↔ values)
   - Build evaluation: buildCV (crit value), buildV (stat totals)

2. Character.hs - CHARACTER DEFINITIONS
   - Character type with: scaling stats, base stats, damage formula
   - Statline: Efficient UArray for stat aggregation
   - Implemented characters: furina, nefer
   - Functions: getFinalStatline, getShownStat, conditionChecker
   - Damage calculators: generic dmgClc + character-specific formulas

3. Weights.hs - GENERATION PROBABILITIES
   - pieceWeight: probability distribution for piece types
   - substatUpgradeNumW: 4 or 5 upgrades (4:1 ratio)
   - substatWeight: probability for each substat appearing
   - pieceMSW: main stat weights per piece type (e.g., Circlet can be CR/CD/HP/ATK/DEF/HB/EM)

4. Generator.hs - ARTIFACT GENERATION
   - generateArtifact: Random artifact with weighted stats
   - generateArtifactForPiece: Generate specific piece type
   - Uses realistic probability distributions from Weights.hs
   - Simulates substat rolling (7-10 value per roll)

5. CharacterBuild.hs - BUILD OPTIMIZATION (CORE ALGORITHM)
   - BuildStrategy: configurable build finder
   - best4pcBuilds: Find top N builds using stat weights
   - paretoFilter/paretoFront: Remove strictly dominated artifacts
   - Iterative weight refinement: calcStatWeightsB/C
   - bestBuild: Main entry point - iteratively improves weights until convergence
   - artValue: Score artifact by weighted sum of stats
   - extendWeights: Auto-add flat stat weights (HPf from HP%, etc.)

6. Progression.hs - ARTIFACT ACQUISITION SIMULATION
   - progression: Track best build as artifacts are acquired over time
   - Uses pareto filtering to reduce search space
   - Returns [(artifactIndex, bestBuildAtThatPoint)]
   - statistics: Aggregate multiple progression runs for analysis

7. UpgradeSimulator.hs - UPGRADE PROBABILITY
   - simulateUpgrades: Test how many new artifacts needed to upgrade each slot
   - Respects on-set vs off-set generation rates
   - Returns upgrade frequency per piece

8. RollProbability.hs - ROLL ANALYSIS
   - analyzeRolls: Estimate artifact upgrade count from substat values
   - Accounts for probability distributions of roll values (7-10)

9. ImportGOOD.hs - DATA IMPORT/EXPORT
   - readGOOD/writeGOOD: Parse JSON artifact data (GOOD format)
   - readGOODForCharacter: Filter by equipped character
   - Converts between JSON and internal Artifact type

10. ExportPlot.hs - VISUALIZATION
    - writePlotData: Export progression data as JSON for Chart.js
    - Format: datasets with x (artifact count), y (damage), hover text

11. Main.hs - SIMULATION RUNNER
    - Concurrent progression simulation (multiple runs)
    - Statistical aggregation across runs
    - Exports plot data for visualization
    - Current config: runs for 'nefer' character

TEST MODULES (test/):
--------------------

12. Tests.hs - TEST SUITE
    - testUpgradeSimulator: Analyze real builds for upgrade difficulty
    - measureProgression: Performance benchmarks
    - compareX: Compare optimization strategies
    - Various validation tests for pareto filtering, partitioning, etc.

13. GeneratorUtils.hs - TEST UTILITIES
    - whileMeasuringTime: Benchmark wrapper
    - withDeterministicRandom: Reproducible tests
    - BuildMaker type aliases and comparison functions
    - Caution: Changes to Generator.hs functions may invalidate recorded benchmarks

14. TestData.hs - BENCHMARK RESULTS
    - Stored results from previous test runs

ALGORITHM FLOW:
---------------

1. WEIGHT INITIALIZATION
   weights = [(stat, initialGuess) for stat in character.scaling]

2. BUILD GENERATION
   builds = best4pcBuilds(weights, depth, setArtifacts, offArtifacts)
   - Partition artifacts by piece type
   - Score each by weighted sum: Σ(stat_value * weight)
   - Take top N per piece
   - Generate all combinations (5-choose-5)
   - Handle 4-piece set bonus (on-set vs off-piece)

3. WEIGHT REFINEMENT
   newWeights = calcStatWeights(character, builds, oldWeights)
   - For each stat, add +34 (4 avg rolls) and measure damage increase
   - Weight ∝ marginal damage increase per roll
   - Repeat until convergence (damage stops improving)

4. PARETO FILTERING (optional optimization)
   filtered = paretoFilter(character, artifacts)
   - For each piece type, remove artifacts that are strictly worse
   - "Worse" = lower on ALL scaling stats
   - Dramatically reduces search space

KEY OPTIMIZATION TECHNIQUES:
---------------------------
- Iterative weight refinement (not one-shot calculation)
- Pareto filtering to prune search space
- Piece-aware depth (Goblet gets 0.5x depth due to rarity)
- Statline caching (UArray for O(1) stat lookup)
- Constraint handling for minimum stat requirements (e.g., ER >= 200%)

WHEN TO REQUEST SPECIFIC FILES:
-------------------------------
- Modifying damage formulas → Character.hs
- Changing optimization logic → CharacterBuild.hs
- Adding new character → Character.hs + possibly Main.hs
- Adjusting generation rates → Weights.hs
- Performance issues → Progression.hs, CharacterBuild.hs
- Testing/validation → Tests.hs
- Data import/export → ImportGOOD.hs
- Visualization → ExportPlot.hs

CURRENT STATE:
--------------
- Main character being optimized: 'nefer' (EM/CR/CD scaling)
- Simulation: 10,000 artifacts per set, multiple concurrent runs
- Output: JSON plot data for damage vs artifact count
- Test suite includes real build analysis from GOOD format exports

COMMON TASKS:
-------------
1. Add new character: Define in Character.hs with damage formula
2. Tune optimization: Adjust depth/strategy in CharacterBuild.hs
3. Change simulation params: Edit Main.hs (artifact counts, runs)
4. Analyze real build: Use testUpgradeSimulator in Tests.hs
5. Benchmark changes: Use GeneratorUtils measurement functions

BUILD SYSTEM:
-------------
- Cabal package (Artifacts.cabal)
- Executable: Artifacts (Main.hs entry point)
- Library: All modules exposed for reuse
- Test suite: Artifacts-test (Tests.hs)
- Threading: -threaded, -O2 optimization
- Dependencies: random, array, containers, extra, bytestring, aeson, async-extra

================================================================================
