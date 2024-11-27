package com.raquo.waypoint

import com.raquo.airstream.core.Signal

import scala.annotation.nowarn
import scala.reflect.ClassTag
import scala.scalajs.js

/**
 * [[SplitRender]] with lazy import view
 */
case class LazyImportSplitRender[Page, View] private(
                                                  underlying: SplitRender[Page, Signal[Option[View]]],
                                                ) extends AnyVal {
  import LazyImportSplitRender.ImportWrapper

  @nowarn
  inline def collect[P <: Page : ClassTag](inline render: P => View): LazyImportSplitRender[Page, View] = {
    val wrapper = ImportWrapper(() => js.dynamicImport(render))
    copy(underlying = underlying.collect[P](p => wrapper.signal.map(r => r.map(_.apply(p)))))
  }

  @nowarn
  inline def collectStatic[P <: Page](page: P)(inline view: => View): LazyImportSplitRender[Page, View] = {
    val wrapper = new ImportWrapper(() => js.dynamicImport(() => view))
    copy(underlying = underlying.collectStatic(page)(wrapper.signal.map(r => r.map(_.apply()))))
  }

  @nowarn
  inline def collectStaticStrict[P <: Page](page: P)(inline view: View): LazyImportSplitRender[Page, View] = {
    val wrapper = new ImportWrapper(() => js.dynamicImport(view))
    copy(underlying = underlying.collectStatic(page)(wrapper.signal))
  }

  @nowarn
  inline def collectSignal[P <: Page : ClassTag](inline render: Signal[P] => View): LazyImportSplitRender[Page, View] = {
    val wrapper = new ImportWrapper(() => js.dynamicImport(render))
    copy(underlying = underlying.collectSignal[P] { signal =>
      wrapper.signal.map(opt => opt.map(fun => fun(signal)))
    })
  }

  def signal: Signal[Option[View]] = underlying.signal.flattenSwitch

  def signalWithLoading(loadingView: => View) = underlying.signal.flattenSwitch.map(x => x.getOrElse(loadingView))
}

object LazyImportSplitRender {
  def apply[Page, View](
                         pageSignal: Signal[Page],
                       ): LazyImportSplitRender[Page, View] = new LazyImportSplitRender(SplitRender(pageSignal))
  
  sealed trait SignalWrapper[View] {
    def signal: Signal[View]
  }

  private class ImportWrapper[View](dynamicImport: () => js.Promise[View]) extends SignalWrapper[Option[View]]:
    private var innerSignal: js.UndefOr[Signal[Option[View]]] = js.undefined

    def signal: Signal[Option[View]] = {
      if (innerSignal.isEmpty) {
        innerSignal = Signal.fromJsPromise(dynamicImport())
      }
      innerSignal.get
    }
  end ImportWrapper
  
}