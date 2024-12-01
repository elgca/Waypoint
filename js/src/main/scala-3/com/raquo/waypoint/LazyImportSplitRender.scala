package com.raquo.waypoint

import com.raquo.airstream.core.Signal
import com.raquo.airstream.state.Val

import scala.annotation.nowarn
import scala.compiletime.constValue
import scala.reflect.ClassTag
import scala.scalajs.js

/**
 * [[SplitRender]] with lazy import view
 */
case class LazyImportSplitRender[Page, View, LazyImport <: Boolean] private (
                                                                              underlying: SplitRender[Page, Signal[Option[View]]]
                                                                            ) extends AnyVal { self =>
  import LazyImportSplitRender.SignalWrapper

  def dynamicImport: LazyImportSplitRender[Page, View, true] =
    self.asInstanceOf[LazyImportSplitRender[Page, View, true]]

  def staticImport: LazyImportSplitRender[Page, View, false] =
    self.asInstanceOf[LazyImportSplitRender[Page, View, false]]

  inline private def signalWrapper[Render](render: Render): SignalWrapper[Option[Render]] =
    SignalWrapper[Render, LazyImport](render)

  @nowarn
  inline def collect[P <: Page: ClassTag](inline render: P => View): LazyImportSplitRender[Page, View, LazyImport] = {
    val wrapper = signalWrapper(render)
    copy(underlying = underlying.collect[P](p => wrapper.signal.map(r => r.map(_.apply(p)))))
  }

  @nowarn
  inline def collectStatic[P <: Page](page: P)(inline view: => View): LazyImportSplitRender[Page, View, LazyImport] = {
    val wrapper = signalWrapper(() => view)
    copy(underlying = underlying.collectStatic(page)(wrapper.signal.map(r => r.map(_.apply()))))
  }

  /** */
  @nowarn
  inline def collectStaticStrict[P <: Page](page: P)(
    inline view: View): LazyImportSplitRender[Page, View, LazyImport] = {
    val wrapper = signalWrapper(view)
    copy(underlying = underlying.collectStatic(page)(wrapper.signal))
  }

  @nowarn
  inline def collectSignal[P <: Page: ClassTag](inline render: Signal[P] => View): LazyImportSplitRender[Page, View, LazyImport] = {
    val wrapper = signalWrapper(render)
    copy(underlying = underlying.collectSignal[P] { signal =>
      wrapper.signal.map(opt => opt.map(fun => fun(signal)))
    })
  }

  def signal: Signal[Option[View]] = underlying.signal.flattenSwitch

  def signalWithLoading(loadingView: => View) = {
    underlying.signal.flattenSwitch.map(x => x.getOrElse(loadingView))
  }
}

object LazyImportSplitRender {

  def apply[Page, View](
                         pageSignal: Signal[Page]
                       ): LazyImportSplitRender[Page, View, true] = new LazyImportSplitRender[Page, View, true](SplitRender(pageSignal))

  sealed trait SignalWrapper[View] {
    def signal: Signal[View]
  }

  object SignalWrapper {
    @nowarn
    inline def apply[View, LazyImport <: Boolean](view: View): SignalWrapper[Option[View]] = {
      if constValue[LazyImport] then new LazyImportWrapper(() => js.dynamicImport(view))
      else new StaticImportWrapper(Val(Some(view)))
    }
  }

  class LazyImportWrapper[View](dynamicImport: () => js.Promise[View]) extends SignalWrapper[Option[View]]:
    private var innerSignal: js.UndefOr[Signal[Option[View]]] = js.undefined

    def signal: Signal[Option[View]] = {
      if (innerSignal.isEmpty) {
        innerSignal = Signal.fromJsPromise(dynamicImport())
      }
      innerSignal.get
    }
  end LazyImportWrapper

  class StaticImportWrapper[View](viewVal: Val[Option[View]]) extends SignalWrapper[Option[View]]:
    def signal: Signal[Option[View]] = viewVal
  end StaticImportWrapper
}