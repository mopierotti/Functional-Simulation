package mopierotti.fsim

object Simulation {
  type Time = Int
  trait Configuration
  trait State
  case class SimulationResult(states:Seq[State])
  trait AggregatedSimulationResults

  def runSimulation(conf:Configuration,
                    startingState:State,
                    endTime:Time,
                    createStepFunction:(Configuration => ((State, Time) => State))):SimulationResult = {
    val stepFunction = createStepFunction(conf)
    val states = (1 to endTime).scanLeft(startingState)(stepFunction)
    SimulationResult(states)
  }

  def runNTimesAndAggregateResults(n:Int,
                                  conf:Configuration,
                                  startingState:State,
                                  endTime:Time,
                                  createStepFunction:(Configuration => ((State, Time) => State)),
                                  createAggregatedResults:(Seq[SimulationResult] => AggregatedSimulationResults)):AggregatedSimulationResults = {
    val results = (1 to n).map(? => runSimulation(conf, startingState, endTime, createStepFunction))
    createAggregatedResults(results)
  }

  case class DefaultAggregatedResults(results:Seq[SimulationResult]) extends AggregatedSimulationResults

  def defaultAggregator(results:Seq[SimulationResult]):AggregatedSimulationResults = {
    DefaultAggregatedResults(results)
  }

}
