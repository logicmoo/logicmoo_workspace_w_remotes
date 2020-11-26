(DEFINE (PROBLEM LOG17)
   (:DOMAIN LOGISTICS-STRIPS)
   (:OBJECTS PACKAGE1 PACKAGE2 PACKAGE3 PACKAGE4 PACKAGE5 PACKAGE6
             PLANE1 TRUCK1-1 LOC1-1 LOC1-2 CITY1 TRUCK2-1 LOC2-1 LOC2-2
             CITY2 TRUCK3-1 LOC3-1 LOC3-2 CITY3 TRUCK4-1 LOC4-1 LOC4-2
             CITY4 TRUCK5-1 LOC5-1 LOC5-2 CITY5 TRUCK6-1 LOC6-1 LOC6-2
             CITY6)
   (:INIT (OBJ PACKAGE1)
          (OBJ PACKAGE2)
          (OBJ PACKAGE3)
          (OBJ PACKAGE4)
          (OBJ PACKAGE5)
          (OBJ PACKAGE6)
          (AIRPLANE PLANE1)
          (TRUCK TRUCK1-1)
          (LOCATION LOC1-1)
          (LOCATION LOC1-2)
          (CITY CITY1)
          (AIRPORT LOC1-1)
          (TRUCK TRUCK2-1)
          (LOCATION LOC2-1)
          (LOCATION LOC2-2)
          (CITY CITY2)
          (AIRPORT LOC2-1)
          (TRUCK TRUCK3-1)
          (LOCATION LOC3-1)
          (LOCATION LOC3-2)
          (CITY CITY3)
          (AIRPORT LOC3-1)
          (TRUCK TRUCK4-1)
          (LOCATION LOC4-1)
          (LOCATION LOC4-2)
          (CITY CITY4)
          (AIRPORT LOC4-1)
          (TRUCK TRUCK5-1)
          (LOCATION LOC5-1)
          (LOCATION LOC5-2)
          (CITY CITY5)
          (AIRPORT LOC5-1)
          (TRUCK TRUCK6-1)
          (LOCATION LOC6-1)
          (LOCATION LOC6-2)
          (CITY CITY6)
          (AIRPORT LOC6-1)
          (IN-CITY LOC1-1 CITY1)
          (IN-CITY LOC1-2 CITY1)
          (IN-CITY LOC2-1 CITY2)
          (IN-CITY LOC2-2 CITY2)
          (IN-CITY LOC3-1 CITY3)
          (IN-CITY LOC3-2 CITY3)
          (IN-CITY LOC4-1 CITY4)
          (IN-CITY LOC4-2 CITY4)
          (IN-CITY LOC5-1 CITY5)
          (IN-CITY LOC5-2 CITY5)
          (IN-CITY LOC6-1 CITY6)
          (IN-CITY LOC6-2 CITY6)
          (AT PLANE1 LOC2-1)
          (AT TRUCK1-1 LOC1-1)
          (AT TRUCK2-1 LOC2-2)
          (AT TRUCK3-1 LOC3-1)
          (AT TRUCK4-1 LOC4-2)
          (AT TRUCK5-1 LOC5-2)
          (AT TRUCK6-1 LOC6-1)
          (AT PACKAGE1 LOC1-2)
          (AT PACKAGE2 LOC2-2)
          (AT PACKAGE3 LOC3-2)
          (AT PACKAGE4 LOC4-2)
          (AT PACKAGE5 LOC5-2)
          (AT PACKAGE6 LOC6-2))
   (:GOAL (AND (AT PACKAGE1 LOC6-2)
               (AT PACKAGE2 LOC6-2)
               (AT PACKAGE3 LOC6-2)
               (AT PACKAGE4 LOC6-2)
               (AT PACKAGE5 LOC6-2)
               (AT PACKAGE6 LOC6-2))))