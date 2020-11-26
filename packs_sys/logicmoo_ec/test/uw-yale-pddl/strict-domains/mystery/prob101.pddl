(DEFINE (PROBLEM MYSTY-A-19)
   (:DOMAIN MYSTERY-TYPED)
   (:OBJECTS SHRIMP CHOCOLATE MARZIPAN PEPPER LEMON PAPAYA ONION
             SCALLOP SNICKERS HAROSET TURKEY CHICKEN FLOUNDER LETTUCE TOFU
             POPOVER WURST - FOOD
             INTOXICATION LOVE STIMULATION SATIETY REST EMPATHY UNDERSTANDING
             LEARNING EXCITEMENT LUBRICITY - PLEASURE
             ANXIETY LACERATION GRIEF ANGER PROSTATITIS ABRASION ANGINA
             SCIATICA BOILS LONELINESS DEPRESSION JEALOUSY HANGOVER DREAD
             ANXIETY-1 ANGINA-2 SCIATICA-4 JEALOUSY-3 BOILS-7 ANGER-8
             LACERATION-5 HANGOVER-6 - PAIN
             GUANABARA QUEBEC MANITOBA ALSACE OREGON ARIZONA - PROVINCE
             MARS SATURN URANUS EARTH - PLANET)
   (:INIT (EATS ONION SHRIMP)
          (CRAVES REST HAROSET)
          (EATS POPOVER TOFU)
          (EATS CHOCOLATE SHRIMP)
          (CRAVES DEPRESSION PAPAYA)
          (EATS HAROSET ONION)
          (LOCALE SCALLOP ALSACE)
          (EATS FLOUNDER POPOVER)
          (EATS LETTUCE WURST)
          (ATTACKS GUANABARA QUEBEC)
          (EATS POPOVER FLOUNDER)
          (CRAVES LEARNING LETTUCE)
          (CRAVES EXCITEMENT TOFU)
          (EATS FLOUNDER CHICKEN)
          (CRAVES HANGOVER ONION)
          (CRAVES ABRASION PEPPER)
          (LOCALE TOFU QUEBEC)
          (EATS PEPPER LEMON)
          (LOCALE HAROSET GUANABARA)
          (CRAVES SCIATICA LEMON)
          (CRAVES DREAD ONION)
          (HARMONY LUBRICITY URANUS)
          (ATTACKS MANITOBA ALSACE)
          (LOCALE SHRIMP GUANABARA)
          (CRAVES ANGER-8 LETTUCE)
          (CRAVES UNDERSTANDING CHICKEN)
          (EATS MARZIPAN PEPPER)
          (CRAVES BOILS-7 LETTUCE)
          (CRAVES ANGINA LEMON)
          (EATS SCALLOP SNICKERS)
          (LOCALE LEMON ARIZONA)
          (LOCALE POPOVER QUEBEC)
          (EATS MARZIPAN SHRIMP)
          (HARMONY REST SATURN)
          (CRAVES JEALOUSY ONION)
          (CRAVES ANGINA-2 HAROSET)
          (EATS ONION PAPAYA)
          (ORBITS SATURN URANUS)
          (EATS SNICKERS WURST)
          (HARMONY INTOXICATION SATURN)
          (ORBITS URANUS EARTH)
          (CRAVES BOILS LEMON)
          (EATS FLOUNDER WURST)
          (HARMONY SATIETY URANUS)
          (CRAVES ANXIETY-1 HAROSET)
          (EATS SHRIMP ONION)
          (CRAVES GRIEF CHOCOLATE)
          (CRAVES LACERATION CHOCOLATE)
          (EATS SHRIMP MARZIPAN)
          (HARMONY LEARNING SATURN)
          (EATS WURST LETTUCE)
          (LOCALE PAPAYA ALSACE)
          (CRAVES JEALOUSY-3 CHICKEN)
          (EATS CHICKEN TOFU)
          (EATS WURST FLOUNDER)
          (LOCALE PEPPER ARIZONA)
          (EATS PAPAYA TURKEY)
          (LOCALE MARZIPAN QUEBEC)
          (ORBITS MARS SATURN)
          (HARMONY LOVE SATURN)
          (CRAVES LACERATION-5 TOFU)
          (EATS SHRIMP CHOCOLATE)
          (CRAVES STIMULATION PEPPER)
          (ATTACKS ALSACE OREGON)
          (EATS SNICKERS SCALLOP)
          (CRAVES SATIETY SCALLOP)
          (EATS LETTUCE CHICKEN)
          (EATS CHICKEN FLOUNDER)
          (ATTACKS QUEBEC MANITOBA)
          (HARMONY UNDERSTANDING URANUS)
          (EATS TOFU CHICKEN)
          (LOCALE WURST GUANABARA)
          (EATS CHICKEN LETTUCE)
          (EATS SCALLOP TURKEY)
          (EATS CHOCOLATE LEMON)
          (CRAVES HANGOVER-6 TOFU)
          (EATS SNICKERS HAROSET)
          (LOCALE TURKEY MANITOBA)
          (EATS LEMON PEPPER)
          (HARMONY STIMULATION SATURN)
          (CRAVES ANXIETY SHRIMP)
          (LOCALE ONION ALSACE)
          (EATS TURKEY SCALLOP)
          (LOCALE LETTUCE GUANABARA)
          (CRAVES INTOXICATION SHRIMP)
          (EATS TURKEY PAPAYA)
          (EATS PAPAYA ONION)
          (CRAVES SCIATICA-4 TURKEY)
          (HARMONY EMPATHY EARTH)
          (HARMONY EXCITEMENT EARTH)
          (LOCALE SNICKERS ALSACE)
          (CRAVES PROSTATITIS PEPPER)
          (CRAVES LUBRICITY WURST)
          (CRAVES LOVE MARZIPAN)
          (EATS ONION HAROSET)
          (LOCALE CHOCOLATE OREGON)
          (EATS LEMON CHOCOLATE)
          (EATS HAROSET SNICKERS)
          (CRAVES ANGER PEPPER)
          (CRAVES LONELINESS PAPAYA)
          (EATS WURST SNICKERS)
          (ATTACKS OREGON ARIZONA)
          (CRAVES EMPATHY TURKEY)
          (LOCALE FLOUNDER QUEBEC)
          (LOCALE CHICKEN ALSACE)
          (EATS PEPPER MARZIPAN)
          (EATS TOFU POPOVER))
   (:GOAL (AND (CRAVES JEALOUSY CHOCOLATE))))