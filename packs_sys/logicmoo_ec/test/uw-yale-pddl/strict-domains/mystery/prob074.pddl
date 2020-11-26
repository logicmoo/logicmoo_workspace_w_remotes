(DEFINE (PROBLEM STRIPS-MYSTY-33)
   (:DOMAIN MYSTERY-STRIPS)
   (:OBJECTS TOFU-16 ARUGULA-5 LAMB-6 SNICKERS-7 CHICKEN-8
             LOBSTER-3 PEAR-4 CUCUMBER-1 YOGURT-2 APPLE BEEF PISTACHIO
             ARUGULA CUCUMBER ENDIVE RICE PEPPER MUTTON ONION MUFFIN SHRIMP
             BAGUETTE SCALLION TOFU PAPAYA GUAVA CANTELOPE HOTDOG TOMATO
             SWEETROLL LETTUCE PORK WONDERBREAD TURKEY LEMON KALE SNICKERS
             YOGURT POPOVER TUNA LOBSTER POTATO CHOCOLATE BACON HAM SCALLOP
             MELON WURST LAMB PEA MARZIPAN HAROSET HAMBURGER ORANGE
             GRAPEFRUIT CHICKEN FLOUNDER COD CHERRY OKRA BROCCOLI PEAR
             LUBRICITY ACHIEVEMENT EMPATHY EXPECTATION STIMULATION
             AESTHETICS ENTERTAINMENT SATISFACTION CURIOSITY LEARNING REST
             EXCITEMENT LONELINESS ANGINA LACERATION DEPRESSION HANGOVER
             BOILS GRIEF ABRASION ANXIETY DREAD PROSTATITIS-15 SCIATICA
             ANGER JEALOUSY PROSTATITIS HANGOVER-14 LONELINESS-10
             ANXIETY-11 DREAD-12 ABRASION-13 JEALOUSY-31 ANGER-32
             LACERATION-9 DEPRESSION-30 SCIATICA-26 BOILS-27 ANGINA-28
             GRIEF-29 PROSTATITIS-22 JEALOUSY-23 ANGER-24 BOILS-25
             DEPRESSION-17 ANGINA-18 HANGOVER-19 LACERATION-20 ANXIETY-21
             ABRASION-61 SCIATICA-62 GRIEF-63 LONELINESS-64 HANGOVER-57
             JEALOUSY-58 BOILS-59 DREAD-60 ANXIETY-53 ANGER-54 DEPRESSION-55
             LONELINESS-56 ANGINA-51 LACERATION-52 PROSTATITIS-48 SCIATICA-49
             GRIEF-50 ABRASION-47 ANGER-44 ANGINA-45 DREAD-46
             LONELINESS-42 BOILS-43 HANGOVER-40 SCIATICA-41 ANXIETY-37
             JEALOUSY-38 DEPRESSION-39 PROSTATITIS-128 DREAD-33 ABRASION-34
             GRIEF-35 LACERATION-36 ANXIETY-123 GRIEF-124 LACERATION-125
             HANGOVER-126 DREAD-127 SCIATICA-119 LONELINESS-120 ABRASION-121
             JEALOUSY-122 PROSTATITIS-114 ANGER-115 ANGINA-116 DEPRESSION-117
             BOILS-118 ANGER-111 ANGINA-112 BOILS-113 ANXIETY-107
             HANGOVER-108 JEALOUSY-109 ABRASION-110 LACERATION-106 DREAD-104
             LONELINESS-105 GRIEF-100 DEPRESSION-101 PROSTATITIS-102
             SCIATICA-103 SCIATICA-98 ABRASION-99 ANXIETY-96 JEALOUSY-97
             ANGER-95 DEPRESSION-90 DREAD-91 LONELINESS-92 PROSTATITIS-93
             BOILS-94 ANGINA-87 HANGOVER-88 LACERATION-89 DEPRESSION-84
             GRIEF-85 GRIEF-86 SCIATICA-79 DREAD-80 ABRASION-81
             HANGOVER-82 JEALOUSY-83 PROSTATITIS-74 BOILS-75 ANGINA-76
             ANGER-77 LONELINESS-78 HANGOVER-70 ANGINA-71 LACERATION-72
             ANXIETY-73 ANXIETY-69 ANGER-66 GRIEF-67 LACERATION-68 BOILS-256
             DEPRESSION-65 PROSTATITIS-252 LONELINESS-253 JEALOUSY-254
             ABRASION-255 BOSNIA GUANABARA KENTUCKY OREGON SATURN VULCAN MARS
             NEPTUNE)
   (:INIT (FOOD TOFU-16)
          (FOOD ARUGULA-5)
          (FOOD LAMB-6)
          (FOOD SNICKERS-7)
          (FOOD CHICKEN-8)
          (FOOD LOBSTER-3)
          (FOOD PEAR-4)
          (FOOD CUCUMBER-1)
          (FOOD YOGURT-2)
          (FOOD APPLE)
          (FOOD BEEF)
          (FOOD PISTACHIO)
          (FOOD ARUGULA)
          (FOOD CUCUMBER)
          (FOOD ENDIVE)
          (FOOD RICE)
          (FOOD PEPPER)
          (FOOD MUTTON)
          (FOOD ONION)
          (FOOD MUFFIN)
          (FOOD SHRIMP)
          (FOOD BAGUETTE)
          (FOOD SCALLION)
          (FOOD TOFU)
          (FOOD PAPAYA)
          (FOOD GUAVA)
          (FOOD CANTELOPE)
          (FOOD HOTDOG)
          (FOOD TOMATO)
          (FOOD SWEETROLL)
          (FOOD LETTUCE)
          (FOOD PORK)
          (FOOD WONDERBREAD)
          (FOOD TURKEY)
          (FOOD LEMON)
          (FOOD KALE)
          (FOOD SNICKERS)
          (FOOD YOGURT)
          (FOOD POPOVER)
          (FOOD TUNA)
          (FOOD LOBSTER)
          (FOOD POTATO)
          (FOOD CHOCOLATE)
          (FOOD BACON)
          (FOOD HAM)
          (FOOD SCALLOP)
          (FOOD MELON)
          (FOOD WURST)
          (FOOD LAMB)
          (FOOD PEA)
          (FOOD MARZIPAN)
          (FOOD HAROSET)
          (FOOD HAMBURGER)
          (FOOD ORANGE)
          (FOOD GRAPEFRUIT)
          (FOOD CHICKEN)
          (FOOD FLOUNDER)
          (FOOD COD)
          (FOOD CHERRY)
          (FOOD OKRA)
          (FOOD BROCCOLI)
          (FOOD PEAR)
          (PLEASURE LUBRICITY)
          (PLEASURE ACHIEVEMENT)
          (PLEASURE EMPATHY)
          (PLEASURE EXPECTATION)
          (PLEASURE STIMULATION)
          (PLEASURE AESTHETICS)
          (PLEASURE ENTERTAINMENT)
          (PLEASURE SATISFACTION)
          (PLEASURE CURIOSITY)
          (PLEASURE LEARNING)
          (PLEASURE REST)
          (PLEASURE EXCITEMENT)
          (PAIN LONELINESS)
          (PAIN ANGINA)
          (PAIN LACERATION)
          (PAIN DEPRESSION)
          (PAIN HANGOVER)
          (PAIN BOILS)
          (PAIN GRIEF)
          (PAIN ABRASION)
          (PAIN ANXIETY)
          (PAIN DREAD)
          (PAIN PROSTATITIS-15)
          (PAIN SCIATICA)
          (PAIN ANGER)
          (PAIN JEALOUSY)
          (PAIN PROSTATITIS)
          (PAIN HANGOVER-14)
          (PAIN LONELINESS-10)
          (PAIN ANXIETY-11)
          (PAIN DREAD-12)
          (PAIN ABRASION-13)
          (PAIN JEALOUSY-31)
          (PAIN ANGER-32)
          (PAIN LACERATION-9)
          (PAIN DEPRESSION-30)
          (PAIN SCIATICA-26)
          (PAIN BOILS-27)
          (PAIN ANGINA-28)
          (PAIN GRIEF-29)
          (PAIN PROSTATITIS-22)
          (PAIN JEALOUSY-23)
          (PAIN ANGER-24)
          (PAIN BOILS-25)
          (PAIN DEPRESSION-17)
          (PAIN ANGINA-18)
          (PAIN HANGOVER-19)
          (PAIN LACERATION-20)
          (PAIN ANXIETY-21)
          (PAIN ABRASION-61)
          (PAIN SCIATICA-62)
          (PAIN GRIEF-63)
          (PAIN LONELINESS-64)
          (PAIN HANGOVER-57)
          (PAIN JEALOUSY-58)
          (PAIN BOILS-59)
          (PAIN DREAD-60)
          (PAIN ANXIETY-53)
          (PAIN ANGER-54)
          (PAIN DEPRESSION-55)
          (PAIN LONELINESS-56)
          (PAIN ANGINA-51)
          (PAIN LACERATION-52)
          (PAIN PROSTATITIS-48)
          (PAIN SCIATICA-49)
          (PAIN GRIEF-50)
          (PAIN ABRASION-47)
          (PAIN ANGER-44)
          (PAIN ANGINA-45)
          (PAIN DREAD-46)
          (PAIN LONELINESS-42)
          (PAIN BOILS-43)
          (PAIN HANGOVER-40)
          (PAIN SCIATICA-41)
          (PAIN ANXIETY-37)
          (PAIN JEALOUSY-38)
          (PAIN DEPRESSION-39)
          (PAIN PROSTATITIS-128)
          (PAIN DREAD-33)
          (PAIN ABRASION-34)
          (PAIN GRIEF-35)
          (PAIN LACERATION-36)
          (PAIN ANXIETY-123)
          (PAIN GRIEF-124)
          (PAIN LACERATION-125)
          (PAIN HANGOVER-126)
          (PAIN DREAD-127)
          (PAIN SCIATICA-119)
          (PAIN LONELINESS-120)
          (PAIN ABRASION-121)
          (PAIN JEALOUSY-122)
          (PAIN PROSTATITIS-114)
          (PAIN ANGER-115)
          (PAIN ANGINA-116)
          (PAIN DEPRESSION-117)
          (PAIN BOILS-118)
          (PAIN ANGER-111)
          (PAIN ANGINA-112)
          (PAIN BOILS-113)
          (PAIN ANXIETY-107)
          (PAIN HANGOVER-108)
          (PAIN JEALOUSY-109)
          (PAIN ABRASION-110)
          (PAIN LACERATION-106)
          (PAIN DREAD-104)
          (PAIN LONELINESS-105)
          (PAIN GRIEF-100)
          (PAIN DEPRESSION-101)
          (PAIN PROSTATITIS-102)
          (PAIN SCIATICA-103)
          (PAIN SCIATICA-98)
          (PAIN ABRASION-99)
          (PAIN ANXIETY-96)
          (PAIN JEALOUSY-97)
          (PAIN ANGER-95)
          (PAIN DEPRESSION-90)
          (PAIN DREAD-91)
          (PAIN LONELINESS-92)
          (PAIN PROSTATITIS-93)
          (PAIN BOILS-94)
          (PAIN ANGINA-87)
          (PAIN HANGOVER-88)
          (PAIN LACERATION-89)
          (PAIN DEPRESSION-84)
          (PAIN GRIEF-85)
          (PAIN GRIEF-86)
          (PAIN SCIATICA-79)
          (PAIN DREAD-80)
          (PAIN ABRASION-81)
          (PAIN HANGOVER-82)
          (PAIN JEALOUSY-83)
          (PAIN PROSTATITIS-74)
          (PAIN BOILS-75)
          (PAIN ANGINA-76)
          (PAIN ANGER-77)
          (PAIN LONELINESS-78)
          (PAIN HANGOVER-70)
          (PAIN ANGINA-71)
          (PAIN LACERATION-72)
          (PAIN ANXIETY-73)
          (PAIN ANXIETY-69)
          (PAIN ANGER-66)
          (PAIN GRIEF-67)
          (PAIN LACERATION-68)
          (PAIN BOILS-256)
          (PAIN DEPRESSION-65)
          (PAIN PROSTATITIS-252)
          (PAIN LONELINESS-253)
          (PAIN JEALOUSY-254)
          (PAIN ABRASION-255)
          (PROVINCE BOSNIA)
          (PROVINCE GUANABARA)
          (PROVINCE KENTUCKY)
          (PROVINCE OREGON)
          (PLANET SATURN)
          (PLANET VULCAN)
          (PLANET MARS)
          (PLANET NEPTUNE)
          (CRAVES SCIATICA-26 ARUGULA)
          (EATS KALE LOBSTER)
          (EATS LETTUCE LEMON)
          (EATS YOGURT-2 GRAPEFRUIT)
          (CRAVES JEALOUSY-23 CUCUMBER)
          (EATS HAMBURGER LEMON)
          (CRAVES ANGINA-51 TOFU)
          (EATS MARZIPAN PORK)
          (CRAVES HANGOVER-82 HAMBURGER)
          (EATS SNICKERS OKRA)
          (EATS RICE GRAPEFRUIT)
          (CRAVES ANXIETY-123 WONDERBREAD)
          (LOCALE CHICKEN-8 GUANABARA)
          (EATS RICE LAMB-6)
          (EATS LEMON HAMBURGER)
          (LOCALE RICE OREGON)
          (LOCALE POPOVER BOSNIA)
          (EATS COD POTATO)
          (CRAVES JEALOUSY-31 BEEF)
          (ORBITS MARS NEPTUNE)
          (CRAVES PROSTATITIS LOBSTER-3)
          (EATS CANTELOPE LAMB)
          (EATS RICE BROCCOLI)
          (EATS BROCCOLI SNICKERS)
          (EATS LETTUCE MARZIPAN)
          (EATS HAROSET SNICKERS)
          (CRAVES JEALOUSY-38 LETTUCE)
          (EATS ORANGE POTATO)
          (LOCALE PEAR-4 OREGON)
          (EATS SCALLOP YOGURT-2)
          (CRAVES GRIEF-100 POTATO)
          (EATS PORK LEMON)
          (CRAVES SCIATICA-41 SWEETROLL)
          (EATS OKRA SWEETROLL)
          (CRAVES GRIEF ARUGULA-5)
          (CRAVES DREAD-104 LOBSTER)
          (CRAVES HANGOVER-126 WONDERBREAD)
          (EATS PEAR CHERRY)
          (EATS CHERRY TOFU)
          (EATS BROCCOLI LAMB)
          (LOCALE TUNA KENTUCKY)
          (CRAVES DREAD-80 HAMBURGER)
          (CRAVES ANXIETY-96 SCALLOP)
          (EATS WURST LOBSTER)
          (EATS TURKEY CUCUMBER)
          (HARMONY ACHIEVEMENT VULCAN)
          (LOCALE GRAPEFRUIT OREGON)
          (EATS HAMBURGER LOBSTER)
          (EATS POPOVER POTATO)
          (CRAVES LACERATION ARUGULA-5)
          (EATS BEEF ARUGULA-5)
          (CRAVES ANGINA-116 KALE)
          (EATS PEA POPOVER)
          (LOCALE PEA BOSNIA)
          (CRAVES ANXIETY-21 RICE)
          (CRAVES HANGOVER-88 MARZIPAN)
          (EATS MARZIPAN SCALLION)
          (LOCALE SNICKERS OREGON)
          (EATS SNICKERS-7 WURST)
          (EATS PEAR CUCUMBER)
          (EATS MUFFIN TUNA)
          (CRAVES ANGER LOBSTER-3)
          (EATS HAROSET PISTACHIO)
          (CRAVES GRIEF-50 PAPAYA)
          (CRAVES ANGER-77 ORANGE)
          (HARMONY EXCITEMENT MARS)
          (CRAVES JEALOUSY-83 HAMBURGER)
          (CRAVES ANGER-24 CUCUMBER)
          (CRAVES BOILS-256 OKRA)
          (CRAVES BOILS-118 KALE)
          (CRAVES BOILS-94 PEA)
          (CRAVES SCIATICA-119 LEMON)
          (CRAVES DREAD SNICKERS-7)
          (EATS PAPAYA SWEETROLL)
          (CRAVES DEPRESSION-101 POTATO)
          (EATS SCALLOP WURST)
          (EATS PISTACHIO HAROSET)
          (EATS YOGURT-2 SCALLOP)
          (EATS LAMB-6 HAM)
          (EATS LOBSTER-3 BROCCOLI)
          (EATS ONION WURST)
          (EATS ONION HAMBURGER)
          (EATS GRAPEFRUIT HAROSET)
          (CRAVES LONELINESS TOFU-16)
          (EATS BROCCOLI RICE)
          (EATS BACON FLOUNDER)
          (CRAVES GRIEF-63 MUTTON)
          (EATS LOBSTER HAMBURGER)
          (LOCALE BROCCOLI GUANABARA)
          (CRAVES ANXIETY LAMB-6)
          (CRAVES DEPRESSION-17 RICE)
          (CRAVES JEALOUSY-254 BROCCOLI)
          (EATS TURKEY MARZIPAN)
          (EATS CHOCOLATE LOBSTER-3)
          (CRAVES DEPRESSION-84 HAROSET)
          (HARMONY EXPECTATION MARS)
          (CRAVES BOILS-25 CUCUMBER)
          (CRAVES DEPRESSION-30 PISTACHIO)
          (CRAVES ANXIETY-107 YOGURT)
          (EATS TUNA BEEF)
          (EATS SNICKERS BROCCOLI)
          (CRAVES JEALOUSY-58 ONION)
          (CRAVES EMPATHY PISTACHIO)
          (EATS PEPPER LEMON)
          (CRAVES LUBRICITY ARUGULA-5)
          (HARMONY STIMULATION MARS)
          (EATS MUTTON SHRIMP)
          (EATS KALE HAMBURGER)
          (EATS APPLE TUNA)
          (LOCALE HAMBURGER GUANABARA)
          (LOCALE PEAR GUANABARA)
          (EATS GRAPEFRUIT ORANGE)
          (LOCALE PORK KENTUCKY)
          (LOCALE COD BOSNIA)
          (CRAVES ANXIETY-53 MUFFIN)
          (EATS ENDIVE PEPPER)
          (LOCALE SHRIMP BOSNIA)
          (CRAVES JEALOUSY-122 LEMON)
          (LOCALE LEMON BOSNIA)
          (EATS SWEETROLL OKRA)
          (EATS CHICKEN-8 SCALLOP)
          (EATS PEAR PORK)
          (HARMONY SATISFACTION NEPTUNE)
          (EATS LEMON CHERRY)
          (EATS PEA COD)
          (CRAVES SCIATICA-79 HAMBURGER)
          (EATS LAMB SCALLOP)
          (CRAVES STIMULATION WONDERBREAD)
          (LOCALE TURKEY OREGON)
          (EATS COD POPOVER)
          (EATS HAROSET OKRA)
          (EATS WURST CANTELOPE)
          (HARMONY LEARNING NEPTUNE)
          (EATS GRAPEFRUIT TOMATO)
          (CRAVES ANGINA-18 RICE)
          (EATS LOBSTER KALE)
          (EATS ORANGE POPOVER)
          (LOCALE BACON BOSNIA)
          (EATS LOBSTER SHRIMP)
          (CRAVES GRIEF-29 ARUGULA)
          (EATS SNICKERS HAROSET)
          (EATS CUCUMBER PORK)
          (EATS BAGUETTE SCALLION)
          (CRAVES ACHIEVEMENT LOBSTER-3)
          (LOCALE HAM GUANABARA)
          (ATTACKS KENTUCKY OREGON)
          (LOCALE LAMB-6 GUANABARA)
          (EATS MARZIPAN LETTUCE)
          (LOCALE PISTACHIO BOSNIA)
          (EATS BEEF TUNA)
          (EATS YOGURT SNICKERS-7)
          (CRAVES LONELINESS-42 HOTDOG)
          (CRAVES ANGINA-112 SNICKERS)
          (LOCALE GUAVA BOSNIA)
          (LOCALE MUFFIN OREGON)
          (EATS ENDIVE PEAR)
          (EATS BEEF WURST)
          (LOCALE HAROSET OREGON)
          (CRAVES DEPRESSION-55 MUFFIN)
          (LOCALE PAPAYA GUANABARA)
          (HARMONY REST MARS)
          (EATS ARUGULA BROCCOLI)
          (CRAVES LONELINESS-78 ORANGE)
          (CRAVES JEALOUSY LOBSTER-3)
          (LOCALE BEEF BOSNIA)
          (CRAVES PROSTATITIS-128 PORK)
          (EATS CANTELOPE HAMBURGER)
          (EATS LAMB YOGURT)
          (LOCALE CANTELOPE BOSNIA)
          (EATS MUTTON MUFFIN)
          (EATS WONDERBREAD HAROSET)
          (CRAVES PROSTATITIS-48 PAPAYA)
          (EATS TURKEY FLOUNDER)
          (CRAVES ABRASION-81 HAMBURGER)
          (HARMONY LUBRICITY NEPTUNE)
          (EATS LAMB BROCCOLI)
          (ORBITS SATURN VULCAN)
          (CRAVES SCIATICA LOBSTER-3)
          (CRAVES HANGOVER-108 YOGURT)
          (EATS GRAPEFRUIT YOGURT-2)
          (CRAVES SATISFACTION LOBSTER)
          (EATS LEMON PORK)
          (EATS BACON SCALLION)
          (CRAVES LACERATION-125 WONDERBREAD)
          (LOCALE POTATO BOSNIA)
          (CRAVES ANGINA TOFU-16)
          (EATS PORK MARZIPAN)
          (CRAVES ABRASION-121 LEMON)
          (EATS GUAVA PEAR-4)
          (LOCALE MUTTON KENTUCKY)
          (LOCALE CHERRY GUANABARA)
          (EATS ORANGE GRAPEFRUIT)
          (LOCALE HOTDOG GUANABARA)
          (CRAVES ABRASION-255 BROCCOLI)
          (CRAVES ANGINA-87 MARZIPAN)
          (CRAVES REST MARZIPAN)
          (EATS HAMBURGER CANTELOPE)
          (EATS TOFU LEMON)
          (EATS GRAPEFRUIT RICE)
          (CRAVES ABRASION-13 YOGURT-2)
          (LOCALE WONDERBREAD GUANABARA)
          (EATS LETTUCE HAMBURGER)
          (CRAVES LONELINESS-92 PEA)
          (EATS TOFU-16 SHRIMP)
          (CRAVES DREAD-12 YOGURT-2)
          (EATS SHRIMP MUTTON)
          (HARMONY CURIOSITY NEPTUNE)
          (LOCALE TOMATO BOSNIA)
          (ATTACKS GUANABARA KENTUCKY)
          (EATS PEAR ENDIVE)
          (CRAVES EXCITEMENT FLOUNDER)
          (CRAVES ANGER-32 BEEF)
          (CRAVES SCIATICA-62 MUTTON)
          (EATS POTATO COD)
          (EATS BEEF CUCUMBER-1)
          (LOCALE WURST BOSNIA)
          (CRAVES LACERATION-36 PORK)
          (ORBITS VULCAN MARS)
          (EATS WURST SNICKERS-7)
          (EATS ARUGULA-5 BEEF)
          (CRAVES BOILS-75 ORANGE)
          (EATS KALE CHERRY)
          (CRAVES ANGER-44 CANTELOPE)
          (EATS LAMB MELON)
          (CRAVES PROSTATITIS-102 POTATO)
          (EATS FLOUNDER CHICKEN)
          (CRAVES HANGOVER-14 PEAR-4)
          (EATS SHRIMP TOFU-16)
          (EATS POTATO WONDERBREAD)
          (CRAVES SCIATICA-103 POTATO)
          (EATS BROCCOLI LOBSTER-3)
          (EATS TUNA MUFFIN)
          (EATS WURST GRAPEFRUIT)
          (CRAVES ANGER-95 MELON)
          (EATS KALE TOFU)
          (EATS CHERRY LOBSTER)
          (EATS SCALLOP CHICKEN-8)
          (EATS BROCCOLI ARUGULA)
          (EATS LEMON LETTUCE)
          (CRAVES PROSTATITIS-114 KALE)
          (EATS HAM LAMB-6)
          (LOCALE OKRA OREGON)
          (EATS WURST SCALLOP)
          (EATS HOTDOG SWEETROLL)
          (EATS BAGUETTE MELON)
          (EATS CANTELOPE WURST)
          (EATS PORK PEAR)
          (EATS PEAR-4 GUAVA)
          (LOCALE YOGURT OREGON)
          (EATS HAM CHICKEN-8)
          (CRAVES LACERATION-9 BEEF)
          (EATS LAMB CANTELOPE)
          (EATS TOFU KALE)
          (CRAVES ANGER-66 CHERRY)
          (CRAVES ANXIETY-69 COD)
          (EATS HAROSET GRAPEFRUIT)
          (EATS PAPAYA PISTACHIO)
          (EATS TUNA PEA)
          (EATS CHERRY TUNA)
          (CRAVES ABRASION LAMB-6)
          (LOCALE TOFU-16 GUANABARA)
          (CRAVES SCIATICA-49 PAPAYA)
          (CRAVES GRIEF-35 PORK)
          (EATS CUCUMBER-1 APPLE)
          (EATS TOFU CHERRY)
          (EATS CHICKEN FLOUNDER)
          (LOCALE LOBSTER GUANABARA)
          (ATTACKS BOSNIA GUANABARA)
          (EATS ARUGULA-5 POPOVER)
          (LOCALE CHOCOLATE BOSNIA)
          (CRAVES PROSTATITIS-252 BROCCOLI)
          (CRAVES EXPECTATION TOMATO)
          (CRAVES ANXIETY-11 YOGURT-2)
          (LOCALE ARUGULA-5 OREGON)
          (EATS PORK CUCUMBER)
          (EATS PISTACHIO PAPAYA)
          (CRAVES CURIOSITY LAMB)
          (CRAVES PROSTATITIS-74 ORANGE)
          (EATS FLOUNDER BACON)
          (CRAVES BOILS-27 ARUGULA)
          (CRAVES LACERATION-89 MARZIPAN)
          (EATS POPOVER WURST)
          (LOCALE YOGURT-2 KENTUCKY)
          (CRAVES BOILS-113 SNICKERS)
          (EATS OKRA HAROSET)
          (EATS LEMON MARZIPAN)
          (LOCALE ARUGULA BOSNIA)
          (EATS CUCUMBER-1 BEEF)
          (LOCALE APPLE KENTUCKY)
          (EATS MELON PEAR-4)
          (EATS SCALLION BAGUETTE)
          (CRAVES DEPRESSION ARUGULA-5)
          (EATS LOBSTER CHERRY)
          (LOCALE MELON KENTUCKY)
          (EATS CHERRY LEMON)
          (EATS CUCUMBER PEAR)
          (EATS LEMON TOFU)
          (EATS BROCCOLI HAROSET)
          (EATS LOBSTER WURST)
          (CRAVES LACERATION-72 FLOUNDER)
          (LOCALE SWEETROLL BOSNIA)
          (LOCALE BAGUETTE BOSNIA)
          (CRAVES DREAD-60 ONION)
          (CRAVES ABRASION-110 YOGURT)
          (CRAVES LONELINESS-64 MUTTON)
          (CRAVES ANGER-115 KALE)
          (CRAVES DEPRESSION-39 LETTUCE)
          (EATS HAMBURGER LETTUCE)
          (EATS CHICKEN ARUGULA)
          (EATS MARZIPAN TURKEY)
          (EATS HAROSET BROCCOLI)
          (CRAVES HANGOVER-40 SWEETROLL)
          (CRAVES HANGOVER-57 ONION)
          (CRAVES DEPRESSION-65 OKRA)
          (LOCALE FLOUNDER OREGON)
          (EATS MELON LAMB)
          (EATS CUCUMBER-1 WURST)
          (CRAVES ANXIETY-37 LETTUCE)
          (HARMONY ENTERTAINMENT VULCAN)
          (LOCALE CUCUMBER-1 OREGON)
          (EATS POPOVER ARUGULA-5)
          (CRAVES AESTHETICS SNICKERS)
          (CRAVES ANXIETY-73 FLOUNDER)
          (CRAVES PROSTATITIS-93 PEA)
          (EATS CHERRY PEAR)
          (CRAVES HANGOVER-70 FLOUNDER)
          (EATS HAMBURGER ONION)
          (EATS SHRIMP LOBSTER)
          (CRAVES LACERATION-106 TUNA)
          (CRAVES ANGER-54 MUFFIN)
          (LOCALE PEPPER OREGON)
          (CRAVES LONELINESS-10 YOGURT-2)
          (EATS POTATO HOTDOG)
          (EATS SWEETROLL HOTDOG)
          (EATS POPOVER COD)
          (LOCALE TOFU BOSNIA)
          (EATS SCALLION MARZIPAN)
          (EATS CHERRY KALE)
          (EATS TUNA CHERRY)
          (CRAVES BOILS ARUGULA-5)
          (CRAVES ANGINA-76 ORANGE)
          (CRAVES BOILS-59 ONION)
          (LOCALE ONION BOSNIA)
          (CRAVES ABRASION-34 PORK)
          (EATS WURST POPOVER)
          (EATS WONDERBREAD POTATO)
          (CRAVES ANGINA-45 CANTELOPE)
          (EATS LEMON PEPPER)
          (EATS POTATO TOMATO)
          (EATS HAROSET WONDERBREAD)
          (EATS YOGURT LAMB)
          (EATS SCALLION BACON)
          (EATS MARZIPAN BACON)
          (CRAVES JEALOUSY-109 YOGURT)
          (EATS FLOUNDER TURKEY)
          (CRAVES LACERATION-20 RICE)
          (CRAVES ABRASION-47 GUAVA)
          (CRAVES ANGINA-28 ARUGULA)
          (LOCALE CUCUMBER OREGON)
          (EATS MELON BAGUETTE)
          (CRAVES GRIEF-67 CHERRY)
          (EATS RICE HAROSET)
          (CRAVES DREAD-33 PORK)
          (EATS WURST CUCUMBER-1)
          (CRAVES ANGINA-71 FLOUNDER)
          (EATS SWEETROLL PAPAYA)
          (CRAVES ABRASION-61 MUTTON)
          (EATS HAMBURGER KALE)
          (LOCALE ORANGE BOSNIA)
          (EATS HAROSET RICE)
          (EATS APPLE CUCUMBER-1)
          (EATS POPOVER ORANGE)
          (EATS CUCUMBER TURKEY)
          (EATS WURST BEEF)
          (LOCALE MARZIPAN BOSNIA)
          (EATS PEA TUNA)
          (EATS OKRA SNICKERS)
          (EATS PEAR-4 MELON)
          (CRAVES HANGOVER-19 RICE)
          (EATS CHICKEN-8 HAM)
          (CRAVES DREAD-46 CANTELOPE)
          (LOCALE KALE OREGON)
          (CRAVES HANGOVER ARUGULA-5)
          (LOCALE SCALLION BOSNIA)
          (LOCALE LETTUCE OREGON)
          (EATS ARUGULA CHICKEN)
          (EATS COD PEA)
          (EATS GRAPEFRUIT WURST)
          (EATS POTATO ORANGE)
          (CRAVES LEARNING PEA)
          (CRAVES DREAD-91 PEA)
          (CRAVES LACERATION-52 TOFU)
          (LOCALE SNICKERS-7 OREGON)
          (EATS SNICKERS-7 YOGURT)
          (CRAVES ANGER-111 SNICKERS)
          (CRAVES DREAD-127 WONDERBREAD)
          (CRAVES GRIEF-85 HAROSET)
          (LOCALE ENDIVE GUANABARA)
          (EATS LOBSTER-3 CHOCOLATE)
          (EATS CHERRY MUFFIN)
          (EATS POPOVER PEA)
          (CRAVES ABRASION-99 CHOCOLATE)
          (CRAVES SCIATICA-98 CHOCOLATE)
          (EATS TOMATO POTATO)
          (CRAVES DEPRESSION-117 KALE)
          (EATS MUFFIN MUTTON)
          (EATS MUFFIN CHERRY)
          (CRAVES GRIEF-86 HAROSET)
          (CRAVES JEALOUSY-97 SCALLOP)
          (EATS BACON MARZIPAN)
          (HARMONY AESTHETICS NEPTUNE)
          (CRAVES LONELINESS-253 BROCCOLI)
          (EATS CHERRY TOFU-16)
          (LOCALE LAMB BOSNIA)
          (EATS HOTDOG POTATO)
          (EATS TUNA APPLE)
          (EATS CHOCOLATE GUAVA)
          (CRAVES DEPRESSION-90 PEA)
          (LOCALE LOBSTER-3 GUANABARA)
          (CRAVES BOILS-43 HOTDOG)
          (CRAVES LONELINESS-56 MUFFIN)
          (CRAVES PROSTATITIS-22 CUCUMBER)
          (CRAVES PROSTATITIS-15 LOBSTER-3)
          (CRAVES LONELINESS-105 LOBSTER)
          (CRAVES LONELINESS-120 LEMON)
          (EATS TOMATO GRAPEFRUIT)
          (CRAVES ENTERTAINMENT YOGURT)
          (EATS MARZIPAN LEMON)
          (HARMONY EMPATHY NEPTUNE)
          (CRAVES GRIEF-124 WONDERBREAD)
          (LOCALE SCALLOP OREGON)
          (EATS PEPPER ENDIVE)
          (EATS TOFU-16 CHERRY)
          (EATS WURST ONION)
          (LOCALE CHICKEN KENTUCKY)
          (EATS SCALLOP LAMB)
          (EATS POTATO POPOVER)
          (EATS GUAVA CHOCOLATE)
          (EATS LAMB-6 RICE)
          (CRAVES LACERATION-68 CHERRY))
   (:GOAL (AND (CRAVES DREAD-104 COD))))