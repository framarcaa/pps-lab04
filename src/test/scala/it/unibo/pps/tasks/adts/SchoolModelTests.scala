package it.unibo.pps.tasks.adts

import org.junit.*
import org.junit.Assert.*
import SchoolModel.*
import it.unibo.pps.u03.extensionmethods.Sequences.Sequence, Sequence.*


class SchoolModelTests:
  val schoolModelADT: SchoolModule = BasicSchoolModule
  import schoolModelADT.*

  @Test def testEmptySchool() =
    val school = emptySchool
    assertEquals(Nil(), school.teachers)
    assertEquals(Nil(), school.courses)

  @Test def testHasElementEmptySchool() =
    val school = emptySchool
    assertEquals(false, school.hasTeacher("John"))
    assertEquals(false, school.hasCourse("Math"))

  @Test def testHasElementNotEmptySchool() =
    val school = emptySchool
    val john = teacher("John")
    val math = course("Math")
    val italian = course("Italian")
    val school2 = school.setTeacherToCourse(john, math)
    assertEquals(true, school2.hasTeacher("John")) // true
    assertEquals(true, school2.hasCourse("Math")) // true
    assertEquals(false, school2.hasCourse("Italian")) // false

  @Test def testSetTeacherToCourse() =
    val school = emptySchool
    val john = teacher("John")
    val mark = teacher("Mark")
    val math = course("Math")
    val italian = course("Italian")
    val school2 = school.setTeacherToCourse(john, math)
    val school3 = school2.setTeacherToCourse(mark, math)
    assertEquals(Cons("Mark", Cons("John", Nil())), school3.teachers) // Cons("John", Nil())
    assertEquals(Cons("Math", Nil()), school2.courses) // Cons("Math", Nil())

  @Test def testCoursesOfATeachers() =
    val school = emptySchool
    val john = teacher("John")
    val math = course("Math")
    val italian = course("Italian")
    val school2 = school.setTeacherToCourse(john, math)
    val school3 = school2.setTeacherToCourse(john, italian)
    assertEquals(Cons("Italian", Cons("Math", Nil())), school3.coursesOfATeacher(john))