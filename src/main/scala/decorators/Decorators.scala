package decorators
import java.time.{Duration, Instant}
import scala.collection.mutable

private[decorators] var statistics_maps = mutable.Map[String,Decorator]()


private[decorators] def toString(o:Any,
                                 iterNPrinted:Int=10):String =
  def map2String(o: scala.collection.Map[_, _]): String =
    if o.size > iterNPrinted then
       s"Map of size ${o.size}. First ${iterNPrinted} elements: \n${o.take(iterNPrinted).toSeq.map((k, v) => s"$k -> ${toString(v)}").mkString("\n")}"
    else
      s"Map of size ${o.size}: \n${o.toSeq.map((k, v) => s"$k -> ${toString(v)}").mkString("\n")}"
  
  def iter2String(o:Iterable[_]):String=
    if o.size > iterNPrinted then
      s"Iterable of size ${o.size}. First ${iterNPrinted} elements:\n${o.take(iterNPrinted).map(toString(_)).mkString("\n")}"
    else
      s"Iterable of size ${o.size}:\n${o.map(toString(_)).mkString("\n")}"
  
  o match
    case o:scala.collection.Map[_,_] => map2String(o)
    case o:Iterable[_] => iter2String(o)
    case o:Any => o.toString

private[decorators] val callingMessage:String=>String = name => 
  s"${Instant.now()}: Calling function '$name' with arguments:"
private[decorators] val returningMessage:(String,Any) => String = (name,result) => 
  s"${Instant.now()}: '$name' returned: ${toString(result)}\n"
private[decorators] val argumentMessage: (Int,Any) => String = (n,arg) => 
  s"#${n}: ${toString(arg)}"


private[decorators] def printMap(map:mutable.Map[_,_],
                                 keyPrinter:Any=>String= key => s"  $key",
                                 valuePrinter:Any=>String = value => value.toString) =
  map.keys.foreach(key => println(s"${keyPrinter(key)} -> ${valuePrinter(map(key))}"))

def printStatistics():Unit =
  statistics_maps.keys.foreach(name => {
    println(s"-------------Statistics-------------")
    printStatistics(name)
  })

def printStatistics(name:String):Unit = {
  println(name+" statistics:")
  val map = statistics_maps(name).toMap
  map.keys.toSeq.sorted.foreach(key => println(s"  $key: ${map(key)}"))
  println("")
}

def printCaches():Unit =
  println(s"-------------Caches-------------")
  statistics_maps.keys.foreach(name => printCaches(name))

def printOutputs():Unit =
  println(s"-------------Outputs/Freq-------------")
  statistics_maps.keys.foreach(name => printOutputs(name))

def printCaches(name:String):Unit =
  println(name+" cache:")
  statistics_maps(name).printCache()
  println("")

def printOutputs(name:String):Unit =
  println(name+" outputs/freq:")
  statistics_maps(name).printOutputsFreq()
  println("")


private[decorators] trait Decorator:
  var hits:Long
  var misses: Long
  var calls: Long
  var maxTime: Long
  var minTime: Long
  var maxStackSize: Long
  var acc:Long
  val name:String
  def printCache():Unit
  def printOutputsFreq():Unit
  def toMap:mutable.Map[String,Long] =
    val map = new mutable.HashMap[String,Long]()
    map.update("memo_hits",hits)
    map.update("memo_misses",misses)
    map.update("calls",calls)
    map.update("time_max",maxTime)
    map.update("time_min",minTime)
    map.update("stack_max_size",maxStackSize)
    map.update("time_acc",acc)
    map.update("time_avg",if calls==0 then acc else acc/calls)
    map

class Decorator1[A,B](val name:String,
                      f:A=>B,
                      @inline stats:Boolean = false,
                      @inline memo:Boolean = false,
                      @inline print:Boolean = false,
                      @inline outputs_freq: Boolean = false) extends Function1[A,B] with Decorator:
  statistics_maps.update(name,this)
  val outputs:mutable.Map[B, Long] =  mutable.Map().withDefaultValue(0L)
  val cache: mutable.Map[A, B] = mutable.Map()
  override def printOutputsFreq(): Unit = printMap(outputs)
  override def printCache(): Unit = printMap(cache)
  var hits = 0L
  var misses = 0L
  var calls = 0L
  var maxTime = 0L
  var minTime = Long.MaxValue
  var maxStackSize = 0L
  var acc = 0L

  override def apply(v1: A): B =
    val start = System.nanoTime
    if stats then
      calls += 1
      maxStackSize = Math.max(maxStackSize,Thread.currentThread().getStackTrace.length)
    if print then
      println(callingMessage(name))
      println(argumentMessage(1,v1))
    try
      val result =
        if memo then
          if cache.contains(v1) then
            hits += 1
            cache(v1)
          else
            misses += 1
            val o = f(v1)
            cache.update(v1, o)
            o
        else
          f(v1)
      if print then println(returningMessage(name,result))
      if outputs_freq then outputs.update(result,outputs(result) + 1)
      result
    finally
      if stats then
        var time = System.nanoTime() - start
        acc += time
        maxTime = Math.max(maxTime, time)
        minTime = Math.min(minTime, time)

class Decorator2[A,B,C](val name:String,
                        f:(A,B)=>C,
                        @inline stats:Boolean = false,
                        @inline memo:Boolean = false,
                        @inline print:Boolean = false,
                        @inline outputs_freq: Boolean = false) extends Function2[A,B,C] with Decorator:
  statistics_maps.update(name,this)
  val outputs:mutable.Map[C, Long] =  mutable.Map().withDefaultValue(0L)
  val cache: mutable.Map[(A, B),C] = mutable.Map()
  override def printOutputsFreq(): Unit = printMap(outputs)
  override def printCache(): Unit = printMap(cache)
  var hits = 0L
  var misses = 0L
  var calls = 0L
  var maxTime = 0L
  var minTime = Long.MaxValue
  var maxStackSize = 0L
  var acc = 0L
  override def apply(v1: A,v2:B): C =
    val start = System.nanoTime
    if stats then
      calls += 1
      maxStackSize = Math.max(maxStackSize,Thread.currentThread().getStackTrace.length)
    if print then
      println(callingMessage(name))
      println(argumentMessage(1,v1))
      println(argumentMessage(2,v2))
    try
      val result =
        if memo then
          if cache.contains((v1,v2)) then
            hits += 1
            cache((v1,v2))
          else
            misses += 1
            val o = f(v1,v2)
            cache.update((v1,v2), o)
            o
        else
          f(v1,v2)
      if print then println(returningMessage(name,result))
      if outputs_freq then outputs.update(result,outputs(result) + 1)
      result
    finally
      if stats then
        var time = System.nanoTime() - start
        acc += time
        maxTime = Math.max(maxTime, time)
        minTime = Math.min(minTime, time)

class Decorator3[A,B,C,D](val name:String,
                          f:(A,B,C)=>D,
                          @inline stats:Boolean = false,
                          @inline memo:Boolean = false,
                          @inline print:Boolean = false,
                          @inline outputs_freq: Boolean = false) extends Function3[A,B,C,D] with Decorator:
  statistics_maps.update(name,this)
  val outputs:mutable.Map[D, Long] =  mutable.Map().withDefaultValue(0L)
  val cache: mutable.Map[(A, B,C),D] = mutable.Map()
  override def printOutputsFreq(): Unit = printMap(outputs)
  override def printCache(): Unit = printMap(cache)
  var hits = 0L
  var misses = 0L
  var calls = 0L
  var maxTime = 0L
  var minTime = Long.MaxValue
  var maxStackSize = 0L
  var acc = 0L
  override def apply(v1: A,v2:B, v3:C): D =
    val start = System.nanoTime
    if stats then
      calls += 1
      maxStackSize = Math.max(maxStackSize,Thread.currentThread().getStackTrace.length)
    if print then
      println(callingMessage(name))
      println(argumentMessage(1,v1))
      println(argumentMessage(2,v2))
      println(argumentMessage(3,v3))
    try
      val result =
        if memo then
          if cache.contains((v1,v2,v3)) then
            hits += 1
            cache((v1,v2,v3))
          else
            misses += 1
            val o = f(v1,v2,v3)
            cache.update((v1,v2,v3), o)
            o
        else
          f(v1,v2,v3)
      if print then println(returningMessage(name,result))
      if outputs_freq then outputs.update(result,outputs(result) + 1)
      result
    finally
      if stats then
        var time = System.nanoTime() - start
        acc += time
        maxTime = Math.max(maxTime, time)
        minTime = Math.min(minTime, time)

class Decorator4[A,B,C,D,E](val name:String,
                            f:(A,B,C,D)=>E,
                            @inline stats:Boolean = false,
                            @inline memo:Boolean = false,
                            @inline print:Boolean = false,
                            @inline outputs_freq: Boolean = false) extends Function4[A,B,C,D,E] with Decorator:
  statistics_maps.update(name,this)
  val outputs:mutable.Map[E, Long] =  mutable.Map().withDefaultValue(0L)
  val cache: mutable.Map[(A, B,C,D),E] = mutable.Map()
  override def printOutputsFreq(): Unit = printMap(outputs)
  override def printCache(): Unit = printMap(cache)
  var hits = 0L
  var misses = 0L
  var calls = 0L
  var maxTime = 0L
  var minTime = Long.MaxValue
  var maxStackSize = 0L
  var acc = 0L
  override def apply(v1: A,v2:B, v3:C, v4:D): E =
    val start = System.nanoTime
    if stats then
      calls += 1
      maxStackSize = Math.max(maxStackSize,Thread.currentThread().getStackTrace.length)
    if print then
      println(callingMessage(name))
      println(argumentMessage(1,v1))
      println(argumentMessage(2,v2))
      println(argumentMessage(3,v3))
      println(argumentMessage(4,v4))
    try
      val result =
        if memo then
          if cache.contains((v1,v2,v3,v4)) then
            hits += 1
            cache((v1,v2,v3,v4))
          else
            misses += 1
            val o = f(v1,v2,v3,v4)
            cache.update((v1,v2,v3,v4), o)
            o
        else
          f(v1,v2,v3,v4)
      if print then println(returningMessage(name,result))
      if outputs_freq then outputs.update(result,outputs(result) + 1)
      result
    finally
      if stats then
        var time = System.nanoTime() - start
        acc += time
        maxTime = Math.max(maxTime, time)
        minTime = Math.min(minTime, time)


class Decorator5[A,B,C,D,E,F](val name:String,
                              f:(A,B,C,D,E)=>F,
                              @inline stats:Boolean = false,
                              @inline memo:Boolean = false,
                              @inline print:Boolean = false,
                              @inline outputs_freq: Boolean = false) extends Function5[A,B,C,D,E,F] with Decorator:
  statistics_maps.update(name,this)
  val outputs:mutable.Map[F, Long] =  mutable.Map().withDefaultValue(0L)
  val cache: mutable.Map[(A, B,C,D,E),F] = mutable.Map()
  override def printOutputsFreq(): Unit = printMap(outputs)
  override def printCache(): Unit =printMap(cache)
  var hits = 0L
  var misses = 0L
  var calls = 0L
  var maxTime = 0L
  var minTime = Long.MaxValue
  var maxStackSize = 0L
  var acc = 0L
  override def apply(v1: A,v2:B, v3:C, v4:D, v5:E): F =
    val start = System.nanoTime
    if stats then
      calls += 1
      maxStackSize = Math.max(maxStackSize,Thread.currentThread().getStackTrace.length)
    if print then
      println(callingMessage(name))
      println(argumentMessage(1,v1))
      println(argumentMessage(2,v2))
      println(argumentMessage(3,v3))
      println(argumentMessage(4,v4))
      println(argumentMessage(5,v5))
    try
      val result =
        if memo then
          if cache.contains((v1,v2,v3,v4,v5)) then
            hits += 1
            cache((v1,v2,v3,v4,v5))
          else
            misses += 1
            val o = f(v1,v2,v3,v4,v5)
            cache.update((v1,v2,v3,v4,v5), o)
            o
        else
          f(v1,v2,v3,v4,v5)
      if print then println(returningMessage(name,result))
      if outputs_freq then outputs.update(result,outputs(result) + 1)
      result
    finally
      if stats then
        var time = System.nanoTime() - start
        acc += time
        maxTime = Math.max(maxTime, time)
        minTime = Math.min(minTime, time)

class Decorator6[A,B,C,D,E,F,G](val name:String,
                                f:(A,B,C,D,E,F)=>G,
                                @inline stats:Boolean = false,
                                @inline memo:Boolean = false,
                                @inline print:Boolean = false,
                                @inline outputs_freq: Boolean = false) extends Function6[A,B,C,D,E,F,G] with Decorator:
  statistics_maps.update(name,this)
  val outputs:mutable.Map[G, Long] =  mutable.Map().withDefaultValue(0L)
  val cache: mutable.Map[(A, B,C,D,E,F),G] = mutable.Map()
  override def printOutputsFreq(): Unit = printMap(outputs)
  override def printCache(): Unit = printMap(cache)
  var hits = 0L
  var misses = 0L
  var calls = 0L
  var maxTime = 0L
  var minTime = Long.MaxValue
  var maxStackSize = 0L
  var acc = 0L
  override def apply(v1: A,v2:B, v3:C, v4:D, v5:E, v6:F): G =
    val start = System.nanoTime
    if stats then
      calls += 1
      maxStackSize = Math.max(maxStackSize,Thread.currentThread().getStackTrace.length)
    if print then
      println(callingMessage(name))
      println(argumentMessage(1,v1))
      println(argumentMessage(2,v2))
      println(argumentMessage(3,v3))
      println(argumentMessage(4,v4))
      println(argumentMessage(5,v5))
      println(argumentMessage(6,v6))
    try
      val result =
        if memo then
          if cache.contains((v1,v2,v3,v4,v5,v6)) then
            hits += 1
            cache((v1,v2,v3,v4,v5,v6))
          else
            misses += 1
            val o = f(v1,v2,v3,v4,v5,v6)
            cache.update((v1,v2,v3,v4,v5,v6), o)
            o
        else
          f(v1,v2,v3,v4,v5,v6)
      if print then println(returningMessage(name,result))
      if outputs_freq then outputs.update(result,outputs(result) + 1)
      result
    finally
      if stats then
        var time = System.nanoTime() - start
        acc += time
        maxTime = Math.max(maxTime, time)
        minTime = Math.min(minTime, time)

def d[A,B](f:A=>B, name:String) =
  new Decorator1[A,B](name,f,stats=false, memo=false, print = true, outputs_freq = false)

def o[A,B](f:A=>B, name:String) =
  new Decorator1[A,B](name,f,stats=false, memo=false, print = false, outputs_freq = true)

def s[A,B](f:A=>B, name:String) =
  new Decorator1[A,B](name,f,stats=true, memo=false, print = false, outputs_freq = false)

def m[A,B](f:A=>B, name:String) =
  new Decorator1[A,B](name,f,stats=false, memo=true, print = false, outputs_freq = false)

def dm[A,B](f:A=>B, name:String) =
  new Decorator1[A,B](name,f,stats=false, memo=true, print = true, outputs_freq = false)

def ms[A,B](f:A=>B, name:String) =
  new Decorator1[A,B](name,f,stats=true, memo=true, print = false, outputs_freq = false)

def ds[A,B](f:A=>B, name:String) =
  new Decorator1[A,B](name,f,stats=true, memo=false, print = true, outputs_freq = false)

def dms[A,B](f:A=>B, name:String) =
  new Decorator1[A,B](name,f,stats=true, memo=true, print = true, outputs_freq = false)

def dmos[A,B](f:A=>B, name:String) =
  new Decorator1[A,B](name,f,stats=true, memo=true, print = true, outputs_freq = true)

def d[A,B,C](f:(A,B)=>C, name:String) =
  new Decorator2[A,B,C](name,f,stats=false, memo=false, print = true, outputs_freq = false)

def o[A,B,C](f:(A,B)=>C, name:String) =
  new Decorator2[A,B,C](name,f,stats=false, memo=false, print = false, outputs_freq = true)

def s[A,B,C](f:(A,B)=>C, name:String) =
  new Decorator2[A,B,C](name,f,stats=true, memo=false, print = false, outputs_freq = false)

def m[A,B,C](f:(A,B)=>C, name:String) =
  new Decorator2[A,B,C](name,f,stats=false, memo=true, print = false, outputs_freq = false)

def ms[A,B,C](f:(A,B)=>C, name:String) =
  new Decorator2[A,B,C](name,f,stats=true, memo=true, print = false, outputs_freq = false)

def dm[A,B,C](f:(A,B)=>C, name:String) =
  new Decorator2[A,B,C](name,f,stats=false, memo=true, print = true, outputs_freq = false)

def ds[A,B,C](f:(A,B)=>C, name:String) =
  new Decorator2[A,B,C](name,f,stats=true, memo=false, print = true, outputs_freq = false)

def dms[A,B,C](f:(A,B)=>C, name:String) =
  new Decorator2[A,B,C](name,f,stats=true, memo=true, print = true, outputs_freq = false)

def dmos[A,B,C](f:(A,B)=>C, name:String) =
  new Decorator2[A,B,C](name,f,stats=true, memo=true, print = true, outputs_freq = true)

def d[A,B,C,D](f:(A,B,C)=>D, name:String) =
  new Decorator3[A,B,C,D](name,f,stats=false, memo=false, print = true, outputs_freq = false)

def o[A,B,C,D](f:(A,B,C)=>D, name:String) =
  new Decorator3[A,B,C,D](name,f,stats=false, memo=false, print = false, outputs_freq = true)

def s[A,B,C,D](f:(A,B,C)=>D, name:String) =
  new Decorator3[A,B,C,D](name,f,stats=true, memo=false, print = false, outputs_freq = false)

def m[A,B,C,D](f:(A,B,C)=>D, name:String) =
  new Decorator3[A,B,C,D](name,f,stats=false, memo=true, print = false, outputs_freq = false)

def ms[A,B,C,D](f:(A,B,C)=>D, name:String) =
  new Decorator3[A,B,C,D](name,f,stats=true, memo=true, print = false, outputs_freq = false)

def dm[A,B,C,D](f:(A,B,C)=>D, name:String) =
  new Decorator3[A,B,C,D](name,f,stats=false, memo=true, print = true, outputs_freq = false)

def ds[A,B,C,D](f:(A,B,C)=>D, name:String) =
  new Decorator3[A,B,C,D](name,f,stats=true, memo=false, print = true, outputs_freq = false)

def dms[A,B,C,D](f:(A,B,C)=>D, name:String) =
  new Decorator3[A,B,C,D](name,f,stats=true, memo=true, print = true, outputs_freq = false)

def dmos[A,B,C,D](f:(A,B,C)=>D, name:String) =
  new Decorator3[A,B,C,D](name,f,stats=true, memo=true, print = true, outputs_freq = true)  

def d[A,B,C,D,E](f:(A,B,C,D)=>E, name:String) =
  new Decorator4[A,B,C,D,E](name,f,stats=false, memo=false, print = true, outputs_freq = false)

def o[A,B,C,D,E](f:(A,B,C,D)=>E, name:String) =
  new Decorator4[A,B,C,D,E](name,f,stats=false, memo=false, print = false, outputs_freq = true)

def s[A,B,C,D,E](f:(A,B,C,D)=>E, name:String) =
  new Decorator4[A,B,C,D,E](name,f,stats=true, memo=false, print = false, outputs_freq = false)

def m[A,B,C,D,E](f:(A,B,C,D)=>E, name:String) =
  new Decorator4[A,B,C,D,E](name,f,stats=false, memo=true, print = false, outputs_freq = false)

def ms[A,B,C,D,E](f:(A,B,C,D)=>E, name:String) =
  new Decorator4[A,B,C,D,E](name,f,stats=true, memo=true, print = false, outputs_freq = false)

def dm[A,B,C,D,E](f:(A,B,C,D)=>E, name:String) =
  new Decorator4[A,B,C,D,E](name,f,stats=false, memo=true, print = true, outputs_freq = false)

def ds[A,B,C,D,E](f:(A,B,C,D)=>E, name:String) =
  new Decorator4[A,B,C,D,E](name,f,stats=true, memo=false, print = true, outputs_freq = false)

def dms[A,B,C,D,E](f:(A,B,C,D)=>E, name:String) =
  new Decorator4[A,B,C,D,E](name,f,stats=true, memo=true, print = true, outputs_freq = false)

def dmos[A,B,C,D,E](f:(A,B,C,D)=>E, name:String) =
  new Decorator4[A,B,C,D,E](name,f,stats=true, memo=true, print = true, outputs_freq = true)

def d[A,B,C,D,E,F](f:(A,B,C,D,E)=>F, name:String) =
  new Decorator5[A,B,C,D,E,F](name,f,stats=false, memo=false, print = true, outputs_freq = false)

def o[A,B,C,D,E,F](f:(A,B,C,D,E)=>F, name:String) =
  new Decorator5[A,B,C,D,E,F](name,f,stats=false, memo=false, print = false, outputs_freq = true)

def s[A,B,C,D,E,F](f:(A,B,C,D,E)=>F, name:String) =
  new Decorator5[A,B,C,D,E,F](name,f,stats=true, memo=false, print = false, outputs_freq = false)

def m[A,B,C,D,E,F](f:(A,B,C,D,E)=>F, name:String) =
  new Decorator5[A,B,C,D,E,F](name,f,stats=false, memo=true, print = false, outputs_freq = false)

def ms[A,B,C,D,E,F](f:(A,B,C,D,E)=>F, name:String) =
  new Decorator5[A,B,C,D,E,F](name,f,stats=true, memo=true, print = false, outputs_freq = false)

def dm[A,B,C,D,E,F](f:(A,B,C,D,E)=>F, name:String) =
  new Decorator5[A,B,C,D,E,F](name,f,stats=false, memo=true, print = true, outputs_freq = false)

def ds[A,B,C,D,E,F](f:(A,B,C,D,E)=>F, name:String) =
  new Decorator5[A,B,C,D,E,F](name,f,stats=true, memo=false, print = true, outputs_freq = false)

def dms[A,B,C,D,E,F](f:(A,B,C,D,E)=>F, name:String) =
  new Decorator5[A,B,C,D,E,F](name,f,stats=true, memo=true, print = true, outputs_freq = false)

def dmos[A,B,C,D,E,F](f:(A,B,C,D,E)=>F, name:String) =
  new Decorator5[A,B,C,D,E,F](name,f,stats=true, memo=true, print = true, outputs_freq = true)

/**
 * Execute n times a supplier that produces a value and measure the min, average and max time
 * @param n number of times the supplier is executed
 * @param supplier a supplier that produces a value
 * @tparam A the type of the value
 * @return a tuple with min, average and max time
 */
def nt[A](n: Int)(supplier: () => A): (Long,Long,Long) =
  def timedcall(f: () => A): (Long,A) =
    val start = System.nanoTime
    val r = f()
    (System.nanoTime() - start,r)
  val xs = (0 to n).map(_ => timedcall(supplier)).map(it => Duration.ofNanos(it._1).toMillis)
  (xs.min,xs.sum/xs.size,xs.max)