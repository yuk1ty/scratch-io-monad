package effect.internal

import effect.IO
import effect.IO.{Delay, FlatMap, Map, Pure, RaiseError}

import scala.collection.mutable
import scala.util.Try
import scala.util.control.NonFatal

object IOExecutor {

  private[this] type Current = IO[Any]

  private[this] type Bind = Any => IO[Any]

  private[this] type CallBack = Either[Throwable, Any] => Unit

  // cats では特別な ArrayStack が実装されているらしい
  private[this] type CallStack = mutable.ArrayStack[Bind]

  def step[A](source: Current): IO[A] = {
    // 現在処理中の IO
    var currentIO = source
    var bindFirst: Bind = null
    var bindRest: CallStack = null
    // Pure や Delay からやってきた値は、Pure と Delay 間で値を再利用可能にするために、
    // 一度この変数の中に unbox されて詰められている。
    var hasUnboxed = false
    var unboxed: AnyRef = null

    do {
      currentIO match {
        case FlatMap(fa, bindNext) => {
          if (bindFirst != null) {
            if (bindRest == null) {
              bindRest = new mutable.ArrayStack()
            }
            bindRest.push(bindFirst)
          }

          bindFirst = bindNext.asInstanceOf[Bind]
          currentIO = fa
        }
        case Pure(value) => {
          unboxed = value.asInstanceOf[AnyRef]
          hasUnboxed = true
        }
        case Delay(thunk) => {
          Try {
            unboxed = thunk().asInstanceOf[AnyRef]
            hasUnboxed = true
            currentIO = null
          }.recover {
            case NonFatal(err) =>
              currentIO = RaiseError(err)
          }.get
        }
        case RaiseError(err) =>
          findErrorHandler(bindFirst, bindRest) match {
            case null => return currentIO.asInstanceOf[IO[A]]
            case bind => {
              val fa = Try(bind.recover(err)).recover {
                case NonFatal(err) => RaiseError(err)
              }.get
              bindFirst = null
              currentIO = fa
            }
          }
        case bindNext @ Map(fa, _, _) => {
          if (bindFirst != null) {
            if (bindRest == null) {
              bindRest = new mutable.ArrayStack()
            }
            bindRest.push(bindFirst)
          }
          bindFirst = bindNext.asInstanceOf[Bind]
          currentIO = fa
        }
      }

      if (hasUnboxed) {
        popNextBind(bindFirst, bindRest) match {
          case null =>
            return (if (currentIO != null) currentIO else Pure(unboxed))
              .asInstanceOf[IO[A]]
          case bind => {
            currentIO = Try(bind(unboxed)).recover {
              case NonFatal(err) => RaiseError(err)
            }.get
            hasUnboxed = false
            unboxed = null
            bindFirst = null
          }
        }
      }
    } while (true)

    // 最後の型の辻褄を合わせるために必要
    // ここに来ることはないが、do-while 文だけだと返り値が Bind にならないので、
    // コンパイルを通すためにそうしているようだ。
    null
  }

  private[this] def findErrorHandler(
      bindFirst: Bind,
      bindRest: CallStack): IOFrame[Any, IO[Any]] = bindFirst match {
    case ref: IOFrame[Any, IO[Any]] => ref
    case _ => {
      if (bindRest == null) {
        null
      } else {
        do {
          val ref = bindRest.pop()
          if (ref == null) {
            return null
          } else if (ref.isInstanceOf[IOFrame[_, _]]) {
            return ref.asInstanceOf[IOFrame[Any, IO[Any]]]
          }
        } while (true)
        null
      }
    }
  }

  private[this] def popNextBind(bindFirst: Bind, bindRest: CallStack): Bind = {
    if (bindFirst != null) {
      return bindFirst
    }

    if (bindRest == null) {
      return null
    }

    do {
      val next = bindRest.pop()
      if (next == null) {
        return null
      } else {
        return next
      }
    } while (true)

    // 最後の型の辻褄を合わせるために必要
    // ここに来ることはないが、do-while 文だけだと返り値が Bind にならないので、
    // コンパイルを通すためにそうしているようだ。
    null
  }
}
