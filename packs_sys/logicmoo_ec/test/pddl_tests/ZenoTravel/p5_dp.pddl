(define (problem p5)

  (:domain zeon)
  (:objects plane1 plane2 plane3 person1 person2 person3 person4 person5 person6 city0 city1 city2 city3 city4 fl0 fl1 fl2 fl3 fl4 fl5 fl6)
  (:init 
    (aircraft plane1)
    (aircraft plane2)
    (aircraft plane3)
    (person person1)
    (person person2)
    (person person3)
    (person person4)
    (person person5)
    (person person6)
    (city city0)
    (city city1)
    (city city2)
    (city city3)
    (city city4)
    (flevel fl0)
    (flevel fl1)
    (flevel fl2)
    (flevel fl3)
    (flevel fl4)
    (flevel fl5)
    (flevel fl6)
    (at plane1 city0)
    (at plane2 city3)
    (at plane3 city0)
    (at person1 city1)
    (at person2 city0)
    (at person3 city2)
    (at person4 city0)
    (at person5 city3)
    (at person6 city4)
    (fuel_level plane1 fl6)
    (fuel_level plane3 fl3)
    (fuel_level plane2 fl0)
    (next fl0 fl1)
    (next fl1 fl2)
    (next fl2 fl3)
    (next fl3 fl4)
    (next fl4 fl5)
    (next fl5 fl6)
    (prev_autstate_1_2)
    (prev_autstate_2_2)
  )
  (:goal (and
    (aut_in_final_1)
    (aut_in_final_2))))