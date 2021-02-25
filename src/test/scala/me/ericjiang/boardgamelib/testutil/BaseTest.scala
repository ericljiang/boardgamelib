package me.ericjiang.boardgamelib.testutil

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.easymock.EasyMockSugar

abstract class BaseTest extends AnyFlatSpec with EasyMockSugar with WhenExecutingThatResets
