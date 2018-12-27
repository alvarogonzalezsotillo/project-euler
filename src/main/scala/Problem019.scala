import java.util.{Date, Calendar}
import java.util.Calendar._
/**
 * Created with IntelliJ IDEA.
 * User: alvaro
 * Date: 20/01/14
 * Time: 11:12
 * To change this template use File | Settings | File Templates.
 */
object Problem19 extends App{
   /*
   You are given the following information, but you may prefer to do some research for yourself.

1 Jan 1900 was a Monday.
Thirty days has September,
April, June and November.
All the rest have thirty-one,
Saving February alone,
Which has twenty-eight, rain or shine.
And on leap years, twenty-nine.
A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
    */

  val c = Calendar.getInstance()

  def date( year: Int, month: Int, day: Int ) = {
    c.set(year,month,day)
    c.getTime
  }
  def dayOfWeek( d: Date ) = {
    c.setTime(d)
    c.get(DAY_OF_WEEK)
  }

  val start = date(1901,JANUARY,1)
  val end = date(2000,DECEMBER,31)

  val dates = Iterator.iterate(start)( d => {
    c.setTime(d)
    c.add( MONTH, 1 )
    c.getTime
  })

  val solution = dates.takeWhile( d => d.getTime <= end.getTime ).count( d => dayOfWeek(d) == SUNDAY)
  println( s"Solution:$solution" )
}
