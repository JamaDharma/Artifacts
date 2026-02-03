module RollProbability (analyzeRolls) where

import Data.List

mrv::Int->[(Int,(Int,Double))]
mrv n = zip [n,n..].map entry.group.sort $ allVariants where
    allVariants = map sum.sequence.replicate n$[7..10]
    normalizer = length allVariants
    entry v = (head v, fromIntegral (length v) / fromIntegral (normalizer))

--(number of rolls,(roll value,probability))
multirollValues :: [(Int, (Int, Double))]
multirollValues = sortOn (snd.snd) (concatMap mrv [1..6])

rollVtoRollN::Int->Int
rollVtoRollN rv = fst.last $ relevant where
    relevant = filter ((==rv).fst.snd) multirollValues


artifactRollDistribution :: Fractional b => Int -> [(Int, ([Int], b))]
artifactRollDistribution n = zip [n,n..].map entry.group.sort $ allDistributions where
    allDistributions = map (sort.map length.group.sort).sequence.replicate n $ [1..4]
    normalizer = length allDistributions
    entry v = (head v, fromIntegral (length v) / fromIntegral (normalizer))


artRollDistribs :: [(Int, ([Int], Double))]
artRollDistribs = concatMap artifactRollDistribution [4,5]

normalize::[(Int, (a, Double))]->[(Int, (a, Double))]
normalize dist = map entry dist where
    entry (n,(k,p)) = (n,(k,p/normalizer))
    normalizer = sum.map (snd.snd) $ dist

rollDistrs :: [Int] -> [[(Int, (Int, Double))]]
rollDistrs = mapM distVariants where
    distVariants rv = normalize.filter ((==rv).fst.snd)$multirollValues

--rolls distribution to upgrades distribution
toUpgrade :: [(Int, (Int, Double))]->([Int], Double)
toUpgrade rd = (sort.filter (/=0).map (sub1.fst)$rd, foldr ((*).snd.snd) 1 rd)
    where sub1 x = x-1

--gives (number of rolls,(distribution of rolls,normalized probability))
distributionDetails :: [Int] -> [(Int, ([Int], Double))]
distributionDetails = normalize.concatMap (upP.toUpgrade).rollDistrs where
    upP u = map (res u).filter ((==fst u).fst.snd)$artRollDistribs
    res (_,rdp) (ln,(rlns,dp)) = (ln,(rlns,rdp*dp))

pOverDistr ::[Int]->[(Int, Double)]
pOverDistr = map res.distributionDetails where
    res (ln,(_,dp)) = (ln, dp)

analyzeRolls::[Int]->Double
analyzeRolls rls = (4*p4+5*p5)/(p4+p5)where
    pod = pOverDistr rls
    p4 = 4*calcP 4
    p5 = 1*calcP 5
    calcP ln = sum.map snd.filter ((==ln).fst) $ pod

kabutoRolls :: [Int]
kabutoRolls = [10,17,7,38]