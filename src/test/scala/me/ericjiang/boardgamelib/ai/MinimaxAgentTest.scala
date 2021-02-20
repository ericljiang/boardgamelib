package me.ericjiang.boardgamelib.ai

import me.ericjiang.boardgamelib.{Action, ActionResult, State}
import org.easymock.EasyMock
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.easymock.EasyMockSugar

class MinimaxAgentTest extends AnyFlatSpec with EasyMockSugar {

  trait TestState extends State[TestState]

  private val heuristic = mock[TestState => Double]
  private val state = mock[TestState]

  "A minimax agent" should "maximize the heuristic function when given terminal actions" in {
    val agent = new MinimaxAgent[TestState](10, heuristic)

    val bestAction = mock[Action[TestState]]
    val worstAction =  mock[Action[TestState]]

    val bestState = mock[TestState]
    val worstState = mock[TestState]

    expecting {
      bestAction.execute(state)
        .andReturn(ActionResult(state = bestState, events = null, availableActions = Set.empty))
      worstAction.execute(state)
        .andReturn(ActionResult(state = worstState, events = null, availableActions = Set.empty))
      heuristic(bestState)
        .andReturn(1)
      heuristic(worstState)
        .andReturn(0)
    }
    whenExecuting(bestAction, worstAction, heuristic) {
      val chosenAction = agent.chooseAction(Set(bestAction, worstAction), state)
      assert(chosenAction == bestAction)
    }
  }

  it should "maximize the heuristic function when depth is exhausted" in {
    val agent = new MinimaxAgent[TestState](1, heuristic)

    val bestAction = mock[Action[TestState]]
    val worstAction =  mock[Action[TestState]]

    val bestState = mock[TestState]
    val worstState = mock[TestState]

    val subsequentAction =  mock[Action[TestState]]

    expecting {
      bestAction.execute(state)
        .andReturn(ActionResult(bestState, null, Set(subsequentAction)))
      worstAction.execute(state)
        .andReturn(ActionResult(worstState, null, Set(subsequentAction)))
      heuristic(bestState)
        .andReturn(1)
      heuristic(worstState)
        .andReturn(0)
    }
    whenExecuting(bestAction, worstAction, heuristic) {
      val chosenAction = agent.chooseAction(Set(bestAction, worstAction), state)
      assert(chosenAction == bestAction)
    }
  }

  override def whenExecuting(mocks: AnyRef*)(fun: => Unit): Unit = {
    super.whenExecuting(mocks: _*)(fun)
    for (m <- mocks)
      EasyMock.reset(m)
  }

}
