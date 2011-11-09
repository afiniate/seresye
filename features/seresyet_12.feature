Feature: Support explicit state passing in an seresye system
  In order to make callbacks more easily accessible and less prone to requiring side effects
  As an Erlang Developer
  I want to be able to have my own per engine state
  and have that state be passed to my rules when they execute

  Scenario: Retrievable  state gets passed to rules
    Given a seresye engine that is initialized with state
    And initialized with data
    When seresye propagation is complete
    Then the per engine state is retrievable
    And contains the data populated by the rules
