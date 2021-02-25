package me.ericjiang.boardgamelib.testutil

import org.easymock.EasyMock
import org.scalatestplus.easymock.EasyMockSugar

trait WhenExecutingThatResets extends EasyMockSugar {
  abstract override def whenExecuting(mocks: AnyRef*)(fun: => Unit): Unit = {
    super.whenExecuting(mocks: _*)(fun)
    for (m <- mocks)
      EasyMock.reset(m)
  }
}
