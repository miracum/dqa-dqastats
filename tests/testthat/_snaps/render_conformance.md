# correct functioning of rendering the conformance results

    Code
      value_conformance_formatted
    Output
      $`Age in years`
      $`Age in years`$conformance_check
      [1] "Conformance check: passed"
      
      $`Age in years`$constraining_rules
      [1] "Constraining values/rules:"
      
      $`Age in years`$kable
           min   max   unit
         <int> <int> <char>
      1:     0   110      a
      
      $`Age in years`$conformance_results
      NULL
      
      
      $`Amount of credit`
      $`Amount of credit`$conformance_check
      [1] "Conformance check: passed"
      
      $`Amount of credit`$constraining_rules
      [1] "Constraining values/rules:"
      
      $`Amount of credit`$kable
           min    max   unit
         <int> <char> <char>
      1:     0    Inf  money
      
      $`Amount of credit`$conformance_results
      NULL
      
      
      $Birthdate
      $Birthdate$conformance_check
      [1] "Conformance check: failed"
      
      $Birthdate$constraining_rules
      [1] "Constraining values/rules:"
      
      $Birthdate$kable
                min        max   format
             <char>     <char>   <char>
      1: 1950-01-01 1989-12-31 %d.%m.%Y
      
      $Birthdate$conformance_results
      [1] "Extrem values are not conform with constraints."
      
      
      $`Credit worthy?`
      $`Credit worthy?`$conformance_check
      [1] "Conformance check: passed"
      
      $`Credit worthy?`$constraining_rules
      [1] "Constraining values/rules: 'yes, no'"
      
      $`Credit worthy?`$kable
      NULL
      
      $`Credit worthy?`$conformance_results
      NULL
      
      
      $`Current bank balance`
      $`Current bank balance`$conformance_check
      [1] "Conformance check: failed"
      
      $`Current bank balance`$constraining_rules
      [1] "Constraining values/rules:"
      
      $`Current bank balance`$kable
            min    max   unit
         <char> <char> <char>
      1:   -Inf    Inf  money
      
      $`Current bank balance`$conformance_results
      [1] "Extrem values are not conform with constraints."
      
      
      $`Date of contact`
      $`Date of contact`$conformance_check
      [1] "Conformance check: failed"
      
      $`Date of contact`$constraining_rules
      [1] "Constraining values/rules:"
      
      $`Date of contact`$kable
                min        max   format
             <char>     <char>   <char>
      1: 2012-01-01 2015-12-31 %d.%m.%Y
      
      $`Date of contact`$conformance_results
      [1] "Extrem values are not conform with constraints."
      
      
      $Forename
      NULL
      
      $Income
      $Income$conformance_check
      [1] "Conformance check: passed"
      
      $Income$constraining_rules
      [1] "Constraining values/rules:"
      
      $Income$kable
           min    max   unit
         <int> <char> <char>
      1:     0    Inf  money
      
      $Income$conformance_results
      NULL
      
      
      $Job
      NULL
      
      $Name
      NULL
      
      $`Person ID`
      NULL
      
      $Sex
      $Sex$conformance_check
      [1] "Conformance check: passed"
      
      $Sex$constraining_rules
      [1] "Constraining values/rules: 'm, f, x'"
      
      $Sex$kable
      NULL
      
      $Sex$conformance_results
      NULL
      
      

