package me.ericjiang.boardgamelib.ai

import me.ericjiang.boardgamelib
import me.ericjiang.boardgamelib.{Action, ActionResult, Game, State}
import me.ericjiang.boardgamelib.ScalaUtils.emptyMultiMap
import org.easymock.EasyMock
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.easymock.EasyMockSugar

class MinimaxAgentTest extends AnyFlatSpec with EasyMockSugar {

  private val game = mock[Game[State]]
  private val heuristic = mock[State => Double]
  private val state = mock[State]

  "A minimax agent" should "maximize the heuristic function when given terminal actions" in {
    val agent = new MinimaxAgent[State, Game[State]](game, 10, heuristic)

    val bestAction = new Action { override def toString = "bestAction" }
    val worstAction = new Action { override def toString = "worstAction" }
    val actions = emptyMultiMap[Class[_ <: Action], Action]
    actions.addBinding(classOf[Action], bestAction)
    actions.addBinding(classOf[Action], worstAction)

    val bestState = new State { override def toString = "bestState" }
    val worstState = new State { override def toString = "worstState" }

    expecting {
      game.submitAction(classOf[Action], bestAction, state)
        .andReturn(boardgamelib.ActionResult(bestState, null, emptyMultiMap[Class[_ <: Action], Action]))
      game.submitAction(classOf[Action], worstAction, state)
        .andReturn(ActionResult(worstState, null, emptyMultiMap[Class[_ <: Action], Action]))
      heuristic(bestState)
        .andReturn(1)
      heuristic(worstState)
        .andReturn(0)
    }
    whenExecuting(game, heuristic) {
      val (_, chosenAction) = agent.chooseAction(actions, state)
      assert(chosenAction == bestAction)
    }
  }

  it should "maximize the heuristic function when depth is exhausted" in {
    val agent = new MinimaxAgent[State, Game[State]](game, 1, heuristic)

    val bestAction = new Action { override def toString = "bestAction" }
    val worstAction = new Action { override def toString = "worstAction" }
    val actions = emptyMultiMap[Class[_ <: Action], Action]
    actions.addBinding(classOf[Action], bestAction)
    actions.addBinding(classOf[Action], worstAction)

    val bestState = new State { override def toString = "bestState" }
    val worstState = new State { override def toString = "worstState" }

    val subsequentAction =  new Action { override def toString = "subsequentAction" }
    val subsequentActions = emptyMultiMap[Class[_ <: Action], Action]
    subsequentActions.addBinding(classOf[Action], subsequentAction)

    expecting {
      game.submitAction(classOf[Action], bestAction, state)
        .andReturn(boardgamelib.ActionResult(bestState, null, subsequentActions))
      game.submitAction(classOf[Action], worstAction, state)
        .andReturn(boardgamelib.ActionResult(worstState, null, subsequentActions))
      heuristic(bestState)
        .andReturn(1)
      heuristic(worstState)
        .andReturn(0)
    }
    whenExecuting(game, heuristic) {
      val (_, chosenAction) = agent.chooseAction(actions, state)
      assert(chosenAction == bestAction)
    }
  }

  override def whenExecuting(mocks: AnyRef*)(fun: => Unit): Unit = {
    super.whenExecuting(mocks: _*)(fun)
    for (m <- mocks)
      EasyMock.reset(m)
  }

}
