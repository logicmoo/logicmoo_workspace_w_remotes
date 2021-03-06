/*

QUERY 1:
instanceOf('WomanUnderLifetimeBRCRisk','Helen',LE).
P=0.123

EXPL:
LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(equivalentClasses(['WomanUnderBRCRisk',intersectionOf(['Woman',someValuesFrom(hasRisk,'BRCRisk')])]),'Helen'),(equivalentClasses(['WomanUnderIncreasedBRCRisk',intersectionOf([someValuesFrom(hasRisk,'IncreasedBRCRisk'),'WomanUnderBRCRisk'])]),'Helen'),(equivalentClasses(['WomanUnderModeratelyIncreasedBRCRisk',intersectionOf(['WomanUnderIncreasedBRCRisk',someValuesFrom(hasRisk,'ModeratelyIncreasedBRCRisk')])]),'Helen'),(subClassOf('PostmenopausalWomanTakingEstrogen','WomanUnderModeratelyIncreasedBRCRisk'),'Helen'),(equivalentClasses(['PostmenopausalWomanTakingEstrogen',intersectionOf(['PostmenopausalWoman',someValuesFrom(hasRiskFactor,'Estrogen')])]),'Helen'),(equivalentClasses(['WomanTakingEstrogen',someValuesFrom(hasRiskFactor,'Estrogen')]),'Helen'),classAssertion('WomanTakingEstrogen','Helen'),classAssertion('PostmenopausalWoman','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(equivalentClasses(['WomanUnderBRCRisk',intersectionOf(['Woman',someValuesFrom(hasRisk,'BRCRisk')])]),'Helen'),(equivalentClasses(['WomanUnderIncreasedBRCRisk',intersectionOf([someValuesFrom(hasRisk,'IncreasedBRCRisk'),'WomanUnderBRCRisk'])]),'Helen'),(subClassOf('WomanUnderModeratelyIncreasedBRCRisk','WomanUnderIncreasedBRCRisk'),'Helen'),(subClassOf('PostmenopausalWomanTakingEstrogen','WomanUnderModeratelyIncreasedBRCRisk'),'Helen'),(equivalentClasses(['PostmenopausalWomanTakingEstrogen',intersectionOf(['PostmenopausalWoman',someValuesFrom(hasRiskFactor,'Estrogen')])]),'Helen'),(equivalentClasses(['WomanTakingEstrogen',someValuesFrom(hasRiskFactor,'Estrogen')]),'Helen'),classAssertion('WomanTakingEstrogen','Helen'),classAssertion('PostmenopausalWoman','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),classAssertion('Woman','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(subClassOf('WomanTakingEstrogen','Woman'),'Helen'),classAssertion('WomanTakingEstrogen','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(equivalentClasses(['PostmenopausalWoman',intersectionOf(['Woman',someValuesFrom(hasRiskFactor,'AfterMenopause')])]),'Helen'),classAssertion('PostmenopausalWoman','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(subClassOf('PostmenopausalWoman','Woman'),'Helen'),classAssertion('PostmenopausalWoman','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(equivalentClasses(['WomanAged3040',intersectionOf(['Woman',someValuesFrom(hasAge,'Age3040')])]),'Helen'),classAssertion('WomanAged3040','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(equivalentClasses(['WomanUnderShortTermBRCRisk',intersectionOf(['Woman',someValuesFrom(hasRisk,'ShortTermBRCRisk')])]),'Helen'),(subClassOf('WomanAged3040','WomanUnderShortTermBRCRisk'),'Helen'),classAssertion('WomanAged3040','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(equivalentClasses(['WomanUnderAbsoluteBRCRisk',intersectionOf(['Woman',someValuesFrom(hasRisk,'AbsoluteBRCRisk')])]),'Helen'),(subClassOf('WomanUnderShortTermBRCRisk','WomanUnderAbsoluteBRCRisk'),'Helen'),(subClassOf('WomanAged3040','WomanUnderShortTermBRCRisk'),'Helen'),classAssertion('WomanAged3040','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(subClassOf('WomanAged3040','Woman'),'Helen'),classAssertion('WomanAged3040','Helen')] ? ;

=================================

QUERY 2:
instanceOf('WomanUnderModeratelyIncreasedBRCRisk','Helen',LE).

EXPL
LE = [(subClassOf('PostmenopausalWomanTakingEstrogen','WomanUnderModeratelyIncreasedBRCRisk'),'Helen'),(equivalentClasses(['PostmenopausalWomanTakingEstrogen',intersectionOf(['PostmenopausalWoman',someValuesFrom(hasRiskFactor,'Estrogen')])]),'Helen'),(equivalentClasses(['WomanTakingEstrogen',someValuesFrom(hasRiskFactor,'Estrogen')]),'Helen'),classAssertion('WomanTakingEstrogen','Helen'),classAssertion('PostmenopausalWoman','Helen')] 
*/


equivalentClasses(['WomanTakingEstrogen',someValuesFrom('hasRiskFactor','Estrogen')]).
equivalentClasses(['WomanTakingProgestin',someValuesFrom('hasRiskFactor','Progestin')]).
equivalentClasses(['AbsoluteBRCRisk',intersectionOf(['BRCRisk',allValuesFrom('riskCategory','AbsoluteRiskCategory')])]).
equivalentClasses(['AfricanAmericanWoman',intersectionOf(['Woman',someValuesFrom('hasRace','AfricanAmerican')])]).
equivalentClasses(['Age50Plus',unionOf(['Age5060','Age70Plus','Age6070'])]).
equivalentClasses(['AshkenaziJewishWoman',intersectionOf([someValuesFrom('hasRace','AshkenaziJew'),'Woman'])]).
equivalentClasses(['BRCRType',intersectionOf(['RiskType',someValuesFrom('riskOf','BreastCancer')])]).
equivalentClasses(['EstrogenProgestin',intersectionOf(['Estrogen','Progestin'])]).
equivalentClasses(['EstrogenTestosterone',intersectionOf(['Testosterone','Estrogen'])]).
equivalentClasses(['IncreasedBRCRisk',intersectionOf(['RelativeBRCRisk',allValuesFrom('riskCategory','IncreasedRiskCategory')])]).
equivalentClasses(['ModeratelyIncreasedBRCRisk',intersectionOf([allValuesFrom('riskCategory','ModerateIncrease'),'IncreasedBRCRisk'])]).
equivalentClasses(['ModeratelyReducedBRCRisk',allValuesFrom('riskCategory','ModerateDecrease')]).
equivalentClasses(['OverweightWoman',intersectionOf(['Woman',someValuesFrom('hasRiskFactor','Overweight')])]).
equivalentClasses(['PersonUnderRisk',intersectionOf(['Person',someValuesFrom('hasRisk','Risk')])]).
equivalentClasses(['PostmenopausalWoman',intersectionOf(['Woman',someValuesFrom('hasRiskFactor','AfterMenopause')])]).
equivalentClasses(['PostmenopausalWomanTakingEstrogen',intersectionOf(['PostmenopausalWoman',someValuesFrom('hasRiskFactor','Estrogen')])]).
equivalentClasses(['PostmenopausalWomanTakingEstrogenAndProgestin',intersectionOf([someValuesFrom('hasRiskFactor','Estrogen'),someValuesFrom('hasRiskFactor','Progestin')])]).
equivalentClasses(['PostmenopausalWomanTakingEstrogenAndTestosterone',intersectionOf([someValuesFrom('hasRiskFactor','Testosterone'),someValuesFrom('hasRiskFactor','Estrogen'),'PostmenopausalWoman'])]).
equivalentClasses(['PostmenopausalWomanTakingProgestin',intersectionOf([someValuesFrom('hasRiskFactor','Progestin'),'PostmenopausalWoman'])]).
equivalentClasses(['PostmenopausalWomanTakingTestosterone',intersectionOf([someValuesFrom('hasRiskFactor','Testosterone'),'PostmenopausalWoman'])]).
equivalentClasses(['PostmenopausalWomanWithHighLevelOfEstrogen',intersectionOf(['PostmenopausalWoman','WomanWithHighLevelOfEstrogen'])]).
equivalentClasses(['PremenopausalWoman',intersectionOf([someValuesFrom('hasRiskFactor','BeforeMenopause'),'Woman'])]).
equivalentClasses(['ReducedBRCRisk',intersectionOf(['RelativeBRCRisk',someValuesFrom('riskCategory','ReducedRiskCategory')])]).
equivalentClasses(['RelativeBRCRisk',intersectionOf(['BRCRisk',allValuesFrom('riskCategory','RelativeRiskCategory')])]).
equivalentClasses(['SeniorWomanWithMotherBRCAffected',intersectionOf([intersectionOf(['Woman',someValuesFrom('hasRiskFactor','MotherAffected')]),someValuesFrom('hasAge','Age50Plus')])]).
equivalentClasses(['StronglyIncreasedBRCRisk',intersectionOf(['IncreasedBRCRisk',allValuesFrom('riskCategory','StrongIncrease')])]).
equivalentClasses(['StronglyReducedBRCRisk',allValuesFrom('riskCategory','StrongDecrease')]).
equivalentClasses(['WeakelyIncreasedBRCRisk',intersectionOf([allValuesFrom('riskCategory','WeakIncrease'),'IncreasedBRCRisk'])]).
equivalentClasses(['WeakelyReducedBRCRisk',allValuesFrom('riskCategory','WeakDecrease')]).
equivalentClasses(['Woman',intersectionOf(['Person',someValuesFrom('hasGender','Female')])]).
equivalentClasses(['WomanAbusingAlcohol',intersectionOf(['Woman',someValuesFrom('hasRiskFactor','Alcohol')])]).
equivalentClasses(['WomanAged2030',someValuesFrom('hasAge','Age2030')]).
equivalentClasses(['WomanAged3040',intersectionOf(['Woman',someValuesFrom('hasAge','Age3040')])]).
equivalentClasses(['WomanAged4050',someValuesFrom('hasAge','Age4050')]).
equivalentClasses(['WomanAged5060',intersectionOf([someValuesFrom('hasAge','Age5060'),'Woman'])]).
equivalentClasses(['WomanAged50Plus',intersectionOf(['Woman',someValuesFrom('hasAge','Age50Plus')])]).
equivalentClasses(['WomanAged6070',someValuesFrom('hasAge','Age6070')]).
equivalentClasses(['WomanAged70Plus',someValuesFrom('hasAge','Age70Plus')]).
equivalentClasses(['WomanAgedUnder20',someValuesFrom('hasAge','AgeUnder20')]).
equivalentClasses(['WomanAgedUnder50',intersectionOf(['Woman',someValuesFrom('hasAge','AgeUnder50')])]).
equivalentClasses(['WomanExposedToRadiationDuringYouth',someValuesFrom('hasRiskFactor','RadiationExposureDuringYouth')]).
equivalentClasses(['WomanHavingFirstPeriodBefore12',intersectionOf(['Woman',someValuesFrom('hasRiskFactor','FirstPeriodBefore12')])]).
equivalentClasses(['WomanLackingExercise',intersectionOf([someValuesFrom('hasRiskFactor','LackOfExercise'),'Woman'])]).
equivalentClasses(['WomanTakingBirthControlPills',intersectionOf(['Woman',someValuesFrom('hasRiskFactor','BirthControlPills')])]).
equivalentClasses(['WomanTakingPostmenopausalHormones',intersectionOf([someValuesFrom('hasRiskFactor','PostmenopausalHormones'),'Woman'])]).
equivalentClasses(['WomanUnderAbsoluteBRCRisk',intersectionOf(['Woman',someValuesFrom('hasRisk','AbsoluteBRCRisk')])]).
equivalentClasses(['WomanUnderBRCRisk',intersectionOf(['Woman',someValuesFrom('hasRisk','BRCRisk')])]).
equivalentClasses(['WomanUnderIncreasedBRCRisk',intersectionOf([someValuesFrom('hasRisk','IncreasedBRCRisk'),'WomanUnderBRCRisk'])]).
equivalentClasses(['WomanUnderLifetimeBRCRisk',intersectionOf(['Woman',someValuesFrom('hasRisk','LifetimeBRCRisk')])]).
equivalentClasses(['WomanUnderModeratelyIncreasedBRCRisk',intersectionOf(['WomanUnderIncreasedBRCRisk',someValuesFrom('hasRisk','ModeratelyIncreasedBRCRisk')])]).
equivalentClasses(['WomanUnderModeratelyReducedBRCRisk',someValuesFrom('hasRisk','ModeratelyReducedBRCRisk')]).
equivalentClasses(['WomanUnderReducedBRCRisk',intersectionOf(['WomanUnderBRCRisk',someValuesFrom('hasRisk','ReducedBRCRisk')])]).
equivalentClasses(['WomanUnderRelativeBRCRisk',intersectionOf([someValuesFrom('hasRisk','RelativeBRCRisk'),'Woman'])]).
equivalentClasses(['WomanUnderShortTermBRCRisk',intersectionOf(['Woman',someValuesFrom('hasRisk','ShortTermBRCRisk')])]).
equivalentClasses(['WomanUnderStronglyIncreasedBRCRisk',intersectionOf([someValuesFrom('hasRisk','StronglyIncreasedBRCRisk'),'WomanUnderIncreasedBRCRisk'])]).
equivalentClasses(['WomanUnderStronglyReducedBRCRisk',someValuesFrom('hasRisk','StronglyReducedBRCRisk')]).
equivalentClasses(['WomanUnderWeakelyIncreasedBRCRisk',intersectionOf(['WomanUnderIncreasedBRCRisk',someValuesFrom('hasRisk','WeakelyIncreasedBRCRisk')])]).
equivalentClasses(['WomanUnderWeakelyReducedBRCRisk',someValuesFrom('hasRisk','WeakelyReducedBRCRisk')]).
equivalentClasses(['WomanWithAtypicalHyperplasia',intersectionOf(['Woman',someValuesFrom('hasRiskFactor','AtypicalHyperplasia')])]).
equivalentClasses(['WomanWithBRCA1Mutation',intersectionOf(['Woman',someValuesFrom('hasRiskFactor','BRCA1Mutation')])]).
equivalentClasses(['WomanWithBRCA2Mutation',intersectionOf([someValuesFrom('hasRiskFactor','BRCA2Mutation'),'Woman'])]).
equivalentClasses(['WomanWithBRCAMutation',intersectionOf([someValuesFrom('hasRiskFactor','BRCAMutation'),'Woman'])]).
equivalentClasses(['WomanWithCarcinomaInSitu',intersectionOf(['Woman',someValuesFrom('hasRiskFactor','CarcinomaInSitu')])]).
equivalentClasses(['WomanWithEarlyFirstChild',intersectionOf(['Woman',someValuesFrom('hasRiskFactor','EarlyFirstChild')])]).
equivalentClasses(['WomanWithEarlyFirstPeriodAndLateMenopause',intersectionOf(['WomanHavingFirstPeriodBefore12','WomanWithLateMenopause'])]).
equivalentClasses(['WomanWithEarlyMenopause',intersectionOf(['Woman',someValuesFrom('hasRiskFactor','EarlyMenopause')])]).
equivalentClasses(['WomanWithFamilyBRCHistory',intersectionOf(['Woman',someValuesFrom('hasRiskFactor','FamilyCancerHistory')])]).
equivalentClasses(['WomanWithHighBoneDensity',intersectionOf([someValuesFrom('hasRiskFactor','HighBreastDensity'),'Woman'])]).
equivalentClasses(['WomanWithHighBreastDensity',intersectionOf([someValuesFrom('hasRiskFactor','HighBreastDensity'),'Woman'])]).
equivalentClasses(['WomanWithHighLevelOfEstrogen',intersectionOf(['Woman',someValuesFrom('hasRiskFactor','HighLevelOfEstrogen')])]).
equivalentClasses(['WomanWithImmediateRelativesBRCAffected',intersectionOf(['Woman',someValuesFrom('hasRiskFactor','TwoImmediateRelativesAffected')])]).
equivalentClasses(['WomanWithLateFirstChild',intersectionOf([someValuesFrom('hasRiskFactor','LateFirstChild'),'Woman'])]).
equivalentClasses(['WomanWithLateMenopause',intersectionOf([someValuesFrom('hasRiskFactor','LateMenopause'),'Woman'])]).
equivalentClasses(['WomanWithMotherAffectedAfterAge60',someValuesFrom('hasRiskFactor','MotherAffectedAfterAge60')]).
equivalentClasses(['WomanWithMotherAffectedBeforeAge60',someValuesFrom('hasRiskFactor','MotherAffectedBeforeAge60')]).
equivalentClasses(['WomanWithMotherBRCAffected',intersectionOf(['Woman',someValuesFrom('hasRiskFactor','MotherAffected')])]).
equivalentClasses(['WomanWithPersonalBRCHistory',intersectionOf([someValuesFrom('hasRiskFactor','PersonalBRCHistory'),'Woman'])]).
equivalentClasses(['WomanWithRiskFactors',intersectionOf(['Woman',someValuesFrom('hasRiskFactor','RiskFactor')])]).
equivalentClasses(['WomanWithUsualHyperplasia',intersectionOf(['Woman',someValuesFrom('hasRiskFactor','UsualHyperplasia')])]).
equivalentClasses(['WomanWithoutBreastfeeding',intersectionOf([someValuesFrom('hasRiskFactor','NoBreastfeeding'),'Woman'])]).
equivalentClasses(['WomanWithoutChildren',intersectionOf(['Woman',someValuesFrom('hasRiskFactor','NoChildren'),someValuesFrom('hasRiskFactor',unionOf(['Age4050','Age50Plus','Age3040']))])]).

disjointClasses(['AfricanAmerican','AshkenaziJew']).
disjointClasses(['Age2030','Age3040']).
disjointClasses(['Age2030','Age4050']).
disjointClasses(['Age2030','Age5060']).
disjointClasses(['Age2030','Age6070']).
disjointClasses(['Age2030','Age70Plus']).
disjointClasses(['Age2030','AgeUnder20']).
disjointClasses(['Age3040','Age4050']).
disjointClasses(['Age3040','Age6070']).
disjointClasses(['Age3040','Age70Plus']).
disjointClasses(['Age3040','AgeUnder20']).
disjointClasses(['Age4050','Age70Plus']).
disjointClasses(['Age5060','Age3040']).
disjointClasses(['Age5060','Age4050']).
disjointClasses(['Age5060','Age6070']).
disjointClasses(['Age5060','Age70Plus']).
disjointClasses(['Age6070','Age4050']).
disjointClasses(['Age6070','Age70Plus']).
disjointClasses(['AgeUnder20','Age4050']).
disjointClasses(['AgeUnder50','Age50Plus']).
disjointClasses(['AtypicalHyperplasia','UsualHyperplasia']).
disjointClasses(['BeforeMenopause','AfterMenopause']).
disjointClasses(['BeforeMenopause','LateMenopause']).
disjointClasses(['EarlyFirstChild','LateFirstChild']).
disjointClasses(['LateMenopause','EarlyMenopause']).
disjointClasses(['ModerateDecrease','WeakDecrease']).
disjointClasses(['MotherAffectedBeforeAge60','MotherAffectedAfterAge60']).
disjointClasses(['PostmenopausalWoman','PremenopausalWoman']).
disjointClasses(['StrongDecrease','ModerateDecrease']).
disjointClasses(['StrongDecrease','WeakDecrease']).
disjointClasses(['StrongIncrease','ModerateIncrease']).
disjointClasses(['StrongIncrease','WeakIncrease']).
disjointClasses(['WeakIncrease','ModerateIncrease']).
disjointClasses(['WomanUnderModeratelyIncreasedBRCRisk','WomanUnderStronglyIncreasedBRCRisk']).
disjointClasses(['WomanUnderModeratelyReducedBRCRisk','WomanUnderStronglyReducedBRCRisk']).
disjointClasses(['WomanUnderModeratelyReducedBRCRisk','WomanUnderWeakelyReducedBRCRisk']).
disjointClasses(['WomanUnderReducedBRCRisk','WomanUnderIncreasedBRCRisk']).
disjointClasses(['WomanUnderStronglyReducedBRCRisk','WomanUnderWeakelyReducedBRCRisk']).
disjointClasses(['WomanUnderWeakelyIncreasedBRCRisk','WomanUnderModeratelyIncreasedBRCRisk']).
disjointClasses(['WomanUnderWeakelyIncreasedBRCRisk','WomanUnderStronglyIncreasedBRCRisk']).
disjointClasses(['WomanWithLateFirstChild','WomanWithEarlyFirstChild']).
disjointClasses(['WomanWithLateFirstChild','WomanWithoutChildren']).
disjointClasses(['WomanWithoutChildren','WomanWithEarlyFirstChild']).
subClassOf('WomanTakingEstrogen','Woman').
subClassOf('WomanTakingProgestin','Woman').
subClassOf('AbsoluteBRCRisk','BRCRisk').
subClassOf('AbsoluteRiskCategory','RiskCategory').
subClassOf('AfricanAmerican','Ethnicity').
subClassOf('AfricanAmericanWoman','Woman').
subClassOf('AfterMenopause','KnownFactor').
subClassOf('Age','KnownFactor').
subClassOf('Age2030','AgeUnder50').
subClassOf('Age3040','AgeUnder50').
subClassOf('Age4050','AgeUnder50').
subClassOf('Age5060','Age').
subClassOf('Age50Plus','Age').
subClassOf('Age6070','Age').
subClassOf('Age70Plus','Age').
subClassOf('AgeUnder20','AgeUnder50').
subClassOf('AgeUnder50','Age').
subClassOf('Alcohol','KnownFactor').
subClassOf('AshkenaziJew','Ethnicity').
subClassOf('AshkenaziJewishWoman','Woman').
subClassOf('AshkenaziJewishWoman','WomanWithBRCAMutation').
subClassOf('AtypicalHyperplasia','BenignBreastDisease').
subClassOf('BRCA1Mutation','BRCAMutation').
subClassOf('BRCA2Mutation','BRCAMutation').
subClassOf('BRCAMutation','InferredFactor').
subClassOf('BRCRisk',intersectionOf([someValuesFrom('riskType','BRCRType'),'Risk'])).
subClassOf('BeforeMenopause','KnownFactor').
subClassOf('BenignBreastDisease','InferredFactor').
subClassOf('BirthControlPills','KnownFactor').
subClassOf('BreastCancer','Cancer').
subClassOf('Cancer','Disease').
subClassOf('CarcinomaInSitu','InferredFactor').
subClassOf('Disease','http://www.w3.org/2002/07/owl#Thing').
subClassOf('EarlyFirstChild','KnownFactor').
subClassOf('EarlyMenopause','KnownFactor').
subClassOf('Estrogen','PostmenopausalHormones').
subClassOf('EstrogenProgestin','PostmenopausalHormones').
subClassOf('EstrogenTestosterone','PostmenopausalHormones').
subClassOf('Ethnicity','http://www.w3.org/2002/07/owl#Thing').
subClassOf('FamilyCancerHistory','KnownFactor').
subClassOf('Female','Gender').
subClassOf('FirstPeriodBefore12','KnownFactor').
subClassOf('Gender','http://www.w3.org/2002/07/owl#Thing').
subClassOf('HighBoneDensity','InferredFactor').
subClassOf('HighBreastDensity','InferredFactor').
subClassOf('HighLevelOfEstrogen','InferredFactor').
subClassOf('IncreasedBRCRisk','RelativeBRCRisk').
subClassOf('IncreasedRiskCategory','RelativeRiskCategory').
subClassOf('InferredFactor','RiskFactor').
subClassOf('KnownFactor','RiskFactor').
subClassOf('LackOfExercise','KnownFactor').
subClassOf('LateFirstChild','KnownFactor').
subClassOf('LateMenopause','KnownFactor').
subClassOf('LifetimeBRCRisk','AbsoluteBRCRisk').
subClassOf('Male','Gender').
subClassOf('ModerateDecrease','ReducedRiskCategory').
subClassOf('ModerateIncrease','IncreasedRiskCategory').
subClassOf('ModeratelyIncreasedBRCRisk','IncreasedBRCRisk').
subClassOf('ModeratelyReducedBRCRisk','ReducedBRCRisk').
subClassOf('MotherAffected','FamilyCancerHistory').
subClassOf('MotherAffectedAfterAge60','MotherAffected').
subClassOf('MotherAffectedBeforeAge60','MotherAffected').
subClassOf('NoBreastfeeding','KnownFactor').
subClassOf('NoChildren','NoBreastfeeding').
subClassOf('Overweight','KnownFactor').
subClassOf('OverweightWoman','Woman').
subClassOf('Person','http://www.w3.org/2002/07/owl#Thing').
subClassOf('Person',someValuesFrom('hasGender','Gender')).
subClassOf('PersonUnderRisk','Person').
subClassOf('PersonalBRCHistory','KnownFactor').
subClassOf('PostmenopausalHormones','KnownFactor').
subClassOf('PostmenopausalWoman','Woman').
subClassOf('PostmenopausalWomanTakingEstrogenAndProgestin','PostmenopausalWoman').
subClassOf('PostmenopausalWomanTakingEstrogenAndTestosterone','PostmenopausalWoman').
subClassOf('PostmenopausalWomanWithHighLevelOfEstrogen','Woman').
subClassOf('PremenopausalWoman','Woman').
subClassOf('Progestin','PostmenopausalHormones').
subClassOf('RadiationExposureDuringYouth','KnownFactor').
subClassOf('ReducedBRCRisk','RelativeBRCRisk').
subClassOf('ReducedRiskCategory','RelativeRiskCategory').
subClassOf('RelativeBRCRisk','BRCRisk').
subClassOf('RelativeRiskCategory','RiskCategory').
subClassOf('Risk','http://www.w3.org/2002/07/owl#Thing').
subClassOf('RiskCategory','http://www.w3.org/2002/07/owl#Thing').
subClassOf('RiskFactor','http://www.w3.org/2002/07/owl#Thing').
subClassOf('RiskFactor',allValuesFrom('relatedToDisease','Disease')).
subClassOf('RiskType',intersectionOf(['http://www.w3.org/2002/07/owl#Thing',someValuesFrom('riskOf','Disease')])).
subClassOf('SeniorWomanWithMotherBRCAffected','Woman').
subClassOf('ShortTermBRCRisk','AbsoluteBRCRisk').
subClassOf('StrongDecrease','ReducedRiskCategory').
subClassOf('StrongIncrease','IncreasedRiskCategory').
subClassOf('StronglyIncreasedBRCRisk','IncreasedBRCRisk').
subClassOf('StronglyReducedBRCRisk','ReducedBRCRisk').
subClassOf('Testosterone','PostmenopausalHormones').
subClassOf('TwoImmediateRelativesAffected','FamilyCancerHistory').
subClassOf('UsualHyperplasia','BenignBreastDisease').
subClassOf('WeakDecrease','ReducedRiskCategory').
subClassOf('WeakIncrease','IncreasedRiskCategory').
subClassOf('WeakelyIncreasedBRCRisk','IncreasedBRCRisk').
subClassOf('WeakelyReducedBRCRisk','ReducedBRCRisk').
subClassOf('Woman','http://www.w3.org/2002/07/owl#Thing').
subClassOf('Woman','WomanUnderLifetimeBRCRisk').
subClassOf('WomanAbusingAlcohol','Woman').
subClassOf('WomanAged2030','Woman').
subClassOf('WomanAged3040','Woman').
subClassOf('WomanAged4050','Woman').
subClassOf('WomanAged5060','Woman').
subClassOf('WomanAged6070','Woman').
subClassOf('WomanAged70Plus','Woman').
subClassOf('WomanAgedUnder20','Woman').
subClassOf('WomanAgedUnder50','WomanWithRiskFactors').
subClassOf('WomanExposedToRadiationDuringYouth','Woman').
subClassOf('WomanHavingFirstPeriodBefore12','Woman').
subClassOf('WomanLackingExercise','Woman').
subClassOf('WomanTakingBirthControlPills','Woman').
subClassOf('WomanTakingPostmenopausalHormones','Woman').
subClassOf('WomanUnderLifetimeBRCRisk','WomanUnderAbsoluteBRCRisk').
subClassOf('WomanUnderModeratelyIncreasedBRCRisk','WomanUnderIncreasedBRCRisk').
subClassOf('WomanUnderModeratelyReducedBRCRisk','WomanUnderReducedBRCRisk').
subClassOf('WomanUnderRelativeBRCRisk','Woman').
subClassOf('WomanUnderShortTermBRCRisk','WomanUnderAbsoluteBRCRisk').
subClassOf('WomanUnderStronglyIncreasedBRCRisk','WomanUnderIncreasedBRCRisk').
subClassOf('WomanUnderStronglyReducedBRCRisk','WomanUnderReducedBRCRisk').
subClassOf('WomanUnderWeakelyIncreasedBRCRisk','WomanUnderIncreasedBRCRisk').
subClassOf('WomanUnderWeakelyReducedBRCRisk','WomanUnderReducedBRCRisk').
subClassOf('WomanWithAtypicalHyperplasia','Woman').
subClassOf('WomanWithBRCA1Mutation','WomanUnderLifetimeBRCRisk').
subClassOf('WomanWithBRCAMutation','WomanWithRiskFactors').
subClassOf('WomanWithBRCAMutation','WomanUnderLifetimeBRCRisk').
subClassOf('WomanWithBRCAMutation','WomanUnderLifetimeBRCRisk').
subClassOf('WomanWithCarcinomaInSitu','Woman').
subClassOf('WomanWithEarlyFirstChild','Woman').
subClassOf('WomanWithEarlyFirstPeriodAndLateMenopause','Woman').
subClassOf('WomanWithEarlyMenopause','PostmenopausalWoman').
subClassOf('WomanWithEarlyMenopause','Woman').
subClassOf('WomanWithFamilyBRCHistory','Woman').
subClassOf('WomanWithHighBoneDensity','Woman').
subClassOf('WomanWithHighBreastDensity','Woman').
subClassOf('WomanWithHighLevelOfEstrogen','Woman').
subClassOf('WomanWithImmediateRelativesBRCAffected','Woman').
subClassOf('WomanWithLateFirstChild','Woman').
subClassOf('WomanWithLateMenopause','PostmenopausalWoman').
subClassOf('WomanWithLateMenopause','Woman').
subClassOf('WomanWithMotherAffectedAfterAge60','WomanWithMotherBRCAffected').
subClassOf('WomanWithMotherAffectedBeforeAge60','WomanWithMotherBRCAffected').
subClassOf('WomanWithMotherBRCAffected','Woman').
subClassOf('WomanWithRiskFactors','Woman').
subClassOf('WomanWithUsualHyperplasia','Woman').
subClassOf('WomanWithoutBreastfeeding','Woman').
subClassOf('WomanWithoutChildren','Woman').
subClassOf('WomanAgedUnder20','WomanUnderShortTermBRCRisk').
subClassOf('WomanAged3040','WomanUnderShortTermBRCRisk').
subClassOf('WomanAged6070','WomanUnderShortTermBRCRisk').
subClassOf('WomanWithMotherAffectedBeforeAge60','WomanUnderModeratelyIncreasedBRCRisk').
subClassOf('WomanWithLateMenopause','WomanUnderModeratelyIncreasedBRCRisk').
subClassOf('PostmenopausalWomanTakingEstrogen','WomanUnderModeratelyIncreasedBRCRisk').
subClassOf('PostmenopausalWomanTakingProgestin','WomanUnderModeratelyIncreasedBRCRisk').
subClassOf('WomanHavingFirstPeriodBefore12','WomanWithHighLevelOfEstrogen').
subPropertyOf('hasAge','hasRiskFactor').
subPropertyOf('willDevelopInLongTerm','willDevelop').
subPropertyOf('willDevelopInShortTerm','willDevelop').
functionalProperty('hasAge').
functionalProperty('hasGender').
functionalProperty('riskCategory').
functionalProperty('riskOf').
functionalProperty('riskType').
functionalProperty('increaseFactor').
propertyDomain('hasAge','Person').
propertyDomain('hasGender','Person').
propertyDomain('hasRace','Person').
propertyDomain('hasRisk','Person').
propertyDomain('hasRiskFactor','Person').
propertyDomain('relatedToDisease','RiskFactor').
propertyDomain('riskCategory','Risk').
propertyDomain('riskOf','RiskType').
propertyDomain('riskType','Risk').
propertyDomain('willDevelop','Person').
propertyDomain('willDevelopInLongTerm','Person').
propertyDomain('willDevelopInShortTerm','Person').
propertyDomain('increaseFactor','RelativeRiskCategory').
propertyRange('hasAge','Age').
propertyRange('hasGender','Gender').
propertyRange('hasRace','Ethnicity').
propertyRange('hasRisk','Risk').
propertyRange('hasRiskFactor','RiskFactor').
propertyRange('relatedToDisease','Disease').
propertyRange('riskCategory','RiskCategory').
propertyRange('riskOf','Disease').
propertyRange('riskType','RiskType').
propertyRange('willDevelop','Disease').
propertyRange('willDevelopInLongTerm','Disease').
propertyRange('willDevelopInShortTerm','Disease').
propertyRange('increaseFactor','http://www.w3.org/2001/XMLSchema#decimal').

classAssertion('Woman','Helen').
classAssertion('WomanTakingEstrogen','Helen').
classAssertion('PostmenopausalWoman','Helen').
classAssertion('WomanAged3040','Helen').
/*
objectProperty('hasAge').
objectProperty('hasGender').
objectProperty('hasRace').
objectProperty('hasRisk').
objectProperty('hasRiskFactor').
objectProperty('relatedToDisease').
objectProperty('riskCategory').
objectProperty('riskOf').
objectProperty('riskType').
objectProperty('willDevelop').
objectProperty('willDevelopInLongTerm').
objectProperty('willDevelopInShortTerm').
dataProperty('increaseFactor').
dataProperty('http://www.w3.org/2006/12/owl11#maxExclusive').
dataProperty('http://www.w3.org/2006/12/owl11#minExclusive').
annotationProperty('http://www.w3.org/2000/01/rdf-schema#label').
annotationProperty('http://www.w3.org/2000/01/rdf-schema#comment').
annotationProperty('https://sites.google.com/a/unife.it/ml/bundle#probability').
class('WomanTakingEstrogen').
class('WomanTakingProgestin').
class('AbsoluteBRCRisk').
class('AbsoluteRiskCategory').
class('AfricanAmerican').
class('AfricanAmericanWoman').
class('AfterMenopause').
class('Age').
class('Age2030').
class('Age3040').
class('Age4050').
class('Age5060').
class('Age50Plus').
class('Age6070').
class('Age70Plus').
class('AgeUnder20').
class('AgeUnder50').
class('Alcohol').
class('AshkenaziJew').
class('AshkenaziJewishWoman').
class('AtypicalHyperplasia').
class('BRCA1Mutation').
class('BRCA2Mutation').
class('BRCAMutation').
class('BRCRType').
class('BRCRisk').
class('BeforeMenopause').
class('BenignBreastDisease').
class('BirthControlPills').
class('BreastCancer').
class('Cancer').
class('CarcinomaInSitu').
class('Disease').
class('EarlyFirstChild').
class('EarlyMenopause').
class('Estrogen').
class('EstrogenProgestin').
class('EstrogenTestosterone').
class('Ethnicity').
class('FamilyCancerHistory').
class('Female').
class('FirstPeriodBefore12').
class('Gender').
class('HighBoneDensity').
class('HighBreastDensity').
class('HighLevelOfEstrogen').
class('IncreasedBRCRisk').
class('IncreasedRiskCategory').
class('InferredFactor').
class('KnownFactor').
class('LackOfExercise').
class('LateFirstChild').
class('LateMenopause').
class('LifetimeBRCRisk').
class('Male').
class('ModerateDecrease').
class('ModerateIncrease').
class('ModeratelyIncreasedBRCRisk').
class('ModeratelyReducedBRCRisk').
class('MotherAffected').
class('MotherAffectedAfterAge60').
class('MotherAffectedBeforeAge60').
class('NoBreastfeeding').
class('NoChildren').
class('Overweight').
class('OverweightWoman').
class('Person').
class('PersonUnderRisk').
class('PersonalBRCHistory').
class('PostmenopausalHormones').
class('PostmenopausalWoman').
class('PostmenopausalWomanTakingEstrogen').
class('PostmenopausalWomanTakingEstrogenAndProgestin').
class('PostmenopausalWomanTakingEstrogenAndTestosterone').
class('PostmenopausalWomanTakingProgestin').
class('PostmenopausalWomanTakingTestosterone').
class('PostmenopausalWomanWithHighLevelOfEstrogen').
class('PremenopausalWoman').
class('Progestin').
class('RadiationExposureDuringYouth').
class('ReducedBRCRisk').
class('ReducedRiskCategory').
class('RelativeBRCRisk').
class('RelativeRiskCategory').
class('Risk').
class('RiskCategory').
class('RiskFactor').
class('RiskType').
class('SeniorWomanWithMotherBRCAffected').
class('ShortTermBRCRisk').
class('StrongDecrease').
class('StrongIncrease').
class('StronglyIncreasedBRCRisk').
class('StronglyReducedBRCRisk').
class('Testosterone').
class('TwoImmediateRelativesAffected').
class('UsualHyperplasia').
class('WeakDecrease').
class('WeakIncrease').
class('WeakelyIncreasedBRCRisk').
class('WeakelyReducedBRCRisk').
class('Woman').
class('WomanAbusingAlcohol').
class('WomanAged2030').
class('WomanAged3040').
class('WomanAged4050').
class('WomanAged5060').
class('WomanAged50Plus').
class('WomanAged6070').
class('WomanAged70Plus').
class('WomanAgedUnder20').
class('WomanAgedUnder50').
class('WomanExposedToRadiationDuringYouth').
class('WomanHavingFirstPeriodBefore12').
class('WomanLackingExercise').
class('WomanTakingBirthControlPills').
class('WomanTakingPostmenopausalHormones').
class('WomanUnderAbsoluteBRCRisk').
class('WomanUnderBRCRisk').
class('WomanUnderIncreasedBRCRisk').
class('WomanUnderLifetimeBRCRisk').
class('WomanUnderModeratelyIncreasedBRCRisk').
class('WomanUnderModeratelyReducedBRCRisk').
class('WomanUnderReducedBRCRisk').
class('WomanUnderRelativeBRCRisk').
class('WomanUnderShortTermBRCRisk').
class('WomanUnderStronglyIncreasedBRCRisk').
class('WomanUnderStronglyReducedBRCRisk').
class('WomanUnderWeakelyIncreasedBRCRisk').
class('WomanUnderWeakelyReducedBRCRisk').
class('WomanWithAtypicalHyperplasia').
class('WomanWithBRCA1Mutation').
class('WomanWithBRCA2Mutation').
class('WomanWithBRCAMutation').
class('WomanWithCarcinomaInSitu').
class('WomanWithEarlyFirstChild').
class('WomanWithEarlyFirstPeriodAndLateMenopause').
class('WomanWithEarlyMenopause').
class('WomanWithFamilyBRCHistory').
class('WomanWithHighBoneDensity').
class('WomanWithHighBreastDensity').
class('WomanWithHighLevelOfEstrogen').
class('WomanWithImmediateRelativesBRCAffected').
class('WomanWithLateFirstChild').
class('WomanWithLateMenopause').
class('WomanWithMotherAffectedAfterAge60').
class('WomanWithMotherAffectedBeforeAge60').
class('WomanWithMotherBRCAffected').
class('WomanWithPersonalBRCHistory').
class('WomanWithRiskFactors').
class('WomanWithUsualHyperplasia').
class('WomanWithoutBreastfeeding').
class('WomanWithoutChildren').
class('http://www.w3.org/2002/07/owl#Thing').
*/

p(subClassOf('AshkenaziJewishWoman','WomanWithBRCAMutation'),0.025).
p(subClassOf('PostmenopausalWomanTakingEstrogen','WomanUnderWeakelyIncreasedBRCRisk'),0.67).
p(subClassOf('PostmenopausalWomanTakingTestosterone','subClassOf WomanUnderWeakelyIncreasedBRCRisk'),0.85).
p(subClassOf('PostmenopausalWomanTakingEstrogenAndProgestin','WomanUnderWeakelyIncreasedBRCRisk'),0.35).
p(subClassOf('WomanWithMotherAffectedAfterAge60','WomanUnderWeakelyIncreasedBRCRisk'),1.0).
p(subClassOf('WomanWithBRCAMutation','WomanUnderLifetimeBRCRisk'),0.85).
p(subClassOf('PostmenopausalWomanTakingProgestin','WomanUnderWeakelyIncreasedBRCRisk'),0.13).
p(subClassOf('WomanWithBRCA1Mutation','WomanUnderLifetimeBRCRisk'),0.8).
p(subClassOf('PostmenopausalWomanTakingEstrogenAndTestosterone','WomanUnderWeakelyIncreasedBRCRisk'),0.21).
p(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),0.123).
