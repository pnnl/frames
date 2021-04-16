c
      program dim
c
      dimension aircon(100,360)
      dimension depwet(100,360)
      dimension depdry(100,360)
      aircon(1,1) = 1.
      depwet(1,1) = 1.
      depdry(1,1) = 1.
      aircon(2,1) = aircon(1,1)
      depwet(2,1) = depwet(1,1)
      depdry(2,1) = depdry(1,1)
      end
