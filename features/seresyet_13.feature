Feature: Support defining rules via module attributes
  In order to seresye more usable
  As an Erlang Developer
  I want to be able define my rules via rule attributes in the module file itself

  Scenario: Rules are defined in the attribute itself
    Given a seresye engine that is initialized with data
    And rules from a module with rules defined via attributes
    When when seresye propagation is complete
    Then the engine runs normally
    And contains the data populated by the rules
