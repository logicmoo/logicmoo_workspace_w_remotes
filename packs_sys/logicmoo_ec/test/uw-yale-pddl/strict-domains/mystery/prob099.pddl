(DEFINE (PROBLEM MYSTY-A-17)
   (:DOMAIN MYSTERY-TYPED)
   (:OBJECTS CUCUMBER TOMATO CANTELOPE PEAR SNICKERS LOBSTER PORK
             CHICKEN TURKEY MUTTON HAROSET BEEF SWEETROLL GRAPEFRUIT TOFU
             HAM HOTDOG BACON ORANGE LETTUCE - FOOD
             LUBRICITY SATISFACTION SATIETY LEARNING STIMULATION CURIOSITY
             - PLEASURE
             PROSTATITIS LACERATION ANXIETY HANGOVER LONELINESS JEALOUSY
             ANGINA ANGER DEPRESSION GRIEF BOILS DREAD-1 DEPRESSION-2
             DREAD ABRASION SCIATICA ABRASION-3 SCIATICA-4 ANGINA-16
             PROSTATITIS-5 ANGER-6 GRIEF-7 LONELINESS-8 LACERATION-13
             HANGOVER-14 BOILS-15 JEALOUSY-12 ANGINA-32 PROSTATITIS-9
             HANGOVER-10 ANXIETY-11 LACERATION-30 SCIATICA-31 ANGER-29
             DEPRESSION-24 GRIEF-25 JEALOUSY-26 ANXIETY-27 BOILS-28 ABRASION-22
             LONELINESS-23 DREAD-21 - PAIN
             GUANABARA OREGON MORAVIA BAVARIA - PROVINCE
             URANUS MARS EARTH JUPITER - PLANET)
   (:INIT (CRAVES ABRASION-3 LOBSTER)
          (EATS TURKEY BEEF)
          (LOCALE BEEF MORAVIA)
          (EATS ORANGE GRAPEFRUIT)
          (CRAVES SCIATICA-4 LOBSTER)
          (CRAVES STIMULATION ORANGE)
          (EATS ORANGE HAM)
          (HARMONY STIMULATION EARTH)
          (EATS TOMATO PEAR)
          (EATS CUCUMBER PORK)
          (CRAVES ANXIETY-11 GRAPEFRUIT)
          (CRAVES BOILS-15 CHICKEN)
          (LOCALE HOTDOG MORAVIA)
          (EATS PEAR LOBSTER)
          (EATS TOMATO SNICKERS)
          (EATS SNICKERS CHICKEN)
          (EATS HAM TOFU)
          (EATS GRAPEFRUIT LETTUCE)
          (CRAVES DREAD-1 SNICKERS)
          (EATS LOBSTER CANTELOPE)
          (EATS GRAPEFRUIT ORANGE)
          (EATS SNICKERS TOMATO)
          (HARMONY LEARNING MARS)
          (EATS LOBSTER PEAR)
          (CRAVES ANGER PEAR)
          (CRAVES JEALOUSY-26 HOTDOG)
          (CRAVES PROSTATITIS CUCUMBER)
          (EATS MUTTON SWEETROLL)
          (EATS HAM SWEETROLL)
          (EATS CHICKEN SNICKERS)
          (EATS PEAR TOMATO)
          (EATS BEEF PEAR)
          (CRAVES LACERATION-13 CHICKEN)
          (LOCALE BACON GUANABARA)
          (CRAVES BOILS-28 HOTDOG)
          (EATS PORK CANTELOPE)
          (EATS LETTUCE HOTDOG)
          (LOCALE LOBSTER OREGON)
          (CRAVES SATISFACTION BEEF)
          (ORBITS MARS EARTH)
          (CRAVES LONELINESS-8 PORK)
          (EATS CANTELOPE LOBSTER)
          (EATS TURKEY LETTUCE)
          (CRAVES PROSTATITIS-9 GRAPEFRUIT)
          (CRAVES PROSTATITIS-5 PORK)
          (ATTACKS MORAVIA BAVARIA)
          (HARMONY CURIOSITY MARS)
          (CRAVES DEPRESSION-2 SNICKERS)
          (CRAVES DEPRESSION-24 HOTDOG)
          (CRAVES ANXIETY CUCUMBER)
          (LOCALE CHICKEN OREGON)
          (EATS HOTDOG LETTUCE)
          (CRAVES BOILS PEAR)
          (EATS HAROSET BACON)
          (CRAVES SCIATICA SNICKERS)
          (EATS CUCUMBER CHICKEN)
          (CRAVES ANGINA CANTELOPE)
          (CRAVES GRIEF-7 PORK)
          (CRAVES HANGOVER-14 CHICKEN)
          (CRAVES SCIATICA-31 TOFU)
          (EATS TOFU BEEF)
          (ATTACKS OREGON MORAVIA)
          (LOCALE TURKEY GUANABARA)
          (EATS SWEETROLL MUTTON)
          (EATS TOFU HAM)
          (LOCALE HAM MORAVIA)
          (EATS BACON HOTDOG)
          (CRAVES LEARNING BACON)
          (CRAVES ABRASION SNICKERS)
          (EATS BACON MUTTON)
          (LOCALE CANTELOPE BAVARIA)
          (EATS HAM ORANGE)
          (EATS HOTDOG BACON)
          (EATS LOBSTER BACON)
          (ORBITS URANUS MARS)
          (LOCALE HAROSET OREGON)
          (CRAVES DREAD SNICKERS)
          (LOCALE ORANGE MORAVIA)
          (CRAVES LONELINESS TOMATO)
          (CRAVES LACERATION CUCUMBER)
          (LOCALE MUTTON GUANABARA)
          (CRAVES ANGINA-32 GRAPEFRUIT)
          (HARMONY SATISFACTION JUPITER)
          (EATS LETTUCE GRAPEFRUIT)
          (CRAVES LUBRICITY PORK)
          (EATS BACON HAROSET)
          (LOCALE TOFU BAVARIA)
          (CRAVES DEPRESSION PEAR)
          (LOCALE SNICKERS MORAVIA)
          (EATS CHICKEN CUCUMBER)
          (CRAVES ANGER-6 PORK)
          (LOCALE TOMATO OREGON)
          (CRAVES GRIEF-25 HOTDOG)
          (EATS HAROSET ORANGE)
          (LOCALE GRAPEFRUIT OREGON)
          (EATS PEAR BEEF)
          (CRAVES ABRASION-22 BACON)
          (LOCALE CUCUMBER OREGON)
          (EATS LETTUCE TURKEY)
          (EATS BEEF TOFU)
          (EATS PORK CUCUMBER)
          (CRAVES HANGOVER CUCUMBER)
          (LOCALE PORK BAVARIA)
          (LOCALE PEAR BAVARIA)
          (CRAVES ANXIETY-27 HOTDOG)
          (CRAVES HANGOVER-10 GRAPEFRUIT)
          (ATTACKS GUANABARA OREGON)
          (CRAVES LACERATION-30 TOFU)
          (CRAVES ANGER-29 HAM)
          (CRAVES DREAD-21 ORANGE)
          (EATS CANTELOPE PORK)
          (EATS ORANGE HAROSET)
          (CRAVES GRIEF PEAR)
          (EATS MUTTON BACON)
          (HARMONY LUBRICITY JUPITER)
          (CRAVES JEALOUSY-12 MUTTON)
          (EATS BEEF TURKEY)
          (HARMONY SATIETY JUPITER)
          (CRAVES SATIETY GRAPEFRUIT)
          (CRAVES LONELINESS-23 BACON)
          (CRAVES JEALOUSY TOMATO)
          (EATS SWEETROLL HAM)
          (CRAVES CURIOSITY LETTUCE)
          (LOCALE LETTUCE GUANABARA)
          (ORBITS EARTH JUPITER)
          (EATS BACON LOBSTER)
          (LOCALE SWEETROLL MORAVIA)
          (CRAVES ANGINA-16 PORK))
   (:GOAL (AND (CRAVES ANGINA HOTDOG)
               (CRAVES HANGOVER-10 HOTDOG))))