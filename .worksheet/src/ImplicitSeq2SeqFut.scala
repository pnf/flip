/**
 * Created with IntelliJ IDEA.
 * User: pnf
 * Date: 6/7/13
 * Time: 10:03 AM
 * To change this template use File | Settings | File Templates.
 */

import scala.collection.generic.CanBuildFrom
import scala.concurrent._
import ExecutionContext.Implicits.global


def fsl[A,B](ii:Seq[A],i0:B,r:(B,A)=>B) : Seq[Future[B]] = ii.scanLeft(future{i0})((f,i)=>f.map( j=>r(j,i) ) )
val y =fsl(1 to 10, 0, (i:Int,j:Int)=>{Thread.sleep(500);i+j})









y.map(_.value)


Thread.sleep(1000)
y.map(_.value)




class SeqFut[A](s : Seq[A]) {
    // like scanLeft, but returns sequence of chained futures
    def scanLeftFut[B](i0:B)(r:(B,A)=>B) : Seq[Future[B]]=
    s.scanLeft(future{i0})((f,i)=>f.map(j=>r(j,i)))
}
object ImplicitSeq2SeqFut {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(791); 
  implicit def Seq2SeqFut[T](s:Seq[T]) = new SeqFut(s);System.out.println("""Seq2SeqFut: [T](s: Seq[T])SeqFut[T]""")}
}



import ImplicitSeq2SeqFut._
val z = (1 to 10).scanLeftFut(0)( (j:Int,i:Int) => {Thread.sleep(500);i+j})








z.map(_.value)

Thread.sleep(1000)
z.map(_.value)
