Feature: Support defining rules via module attributes
  In order to eresye more usable
  As an Erlang Developer
  I want to be able define my rules via rule attributes in the module file itself

  Scenario: Rules are defined in the attribute itself
    Given an eresye engine that is initialized with data
    And rules from a module with rules defined via attributes
    When when eresye propagation is complete
    Then the engine runs normally
    And contains the data populated by the rules


