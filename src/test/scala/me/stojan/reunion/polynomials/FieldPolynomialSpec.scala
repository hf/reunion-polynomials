package me.stojan.reunion.polynomials

import me.stojan.reunion.structure.{ Field, FieldDescriptor }
import me.stojan.reunion.structure.concrete.PrimeField

class FieldPolynomialSpec extends ReunionPolynomialsSpec {
  implicit val primeFieldDescriptor: FieldDescriptor[BigInt] = PrimeField.descriptor(11)

  "FieldPolynomial" should "add [+ PF(11, 1)x^1, 0.] + [+ PF(11, 1)x^1, 0.]" in {
    val a = FieldPolynomial(1, PrimeField(1))
    val b = FieldPolynomial(1, PrimeField(1))

    (a + b) should be (FieldPolynomial(1, PrimeField(2)))
  }

  it should "subtract [+ PF(11, 1)x^1, 0.] - [+ PF(11, 1)x^1, 0.]" in {
    val a = FieldPolynomial(1, PrimeField(1))
    val b = FieldPolynomial(1, PrimeField(1))

    (a - b) should be (a + (-b))
  }

  it should "multiply [+ PF(11, 1)x^1, 0.] * [+ PF(11, 1)x^0, 0.]" in {
    val a = FieldPolynomial(1, PrimeField(1))
    val b = FieldPolynomial(0, PrimeField(1))

    (a * b) should be (a)
  }

  it should "divide [+ PF(11, 1)x^1, 0.] / [+ PF(11, 1)x^0, 0.]" in {
    val a = FieldPolynomial(1, PrimeField(1))
    val b = FieldPolynomial(0, PrimeField(1))

    (a / b) should be ((a, FieldPolynomial()))
  }

  it should "say that [+ PF(11, 1)x^0, 0.] is 1" in {
    (FieldPolynomial.isOne(FieldPolynomial(0, PrimeField(1)))) should be (true)
  }

  it should "say that [+ PF(11, 1)x^1, 0.] is not 1" in {
    (FieldPolynomial.isOne(FieldPolynomial(1, PrimeField(1)))) should be (false)
  }

  it should "say that [+ PF(11, 2)x^0, 0.] is not 1" in {
    (FieldPolynomial.isOne(FieldPolynomial(1, PrimeField(2)))) should be (false)
  }

  it should "say that 0. is not 1" in {
    (FieldPolynomial.isOne(FieldPolynomial())) should be (false)
  }

  it should "say that [+ PF(11, 1)x^3, 0.] is not 0" in {
    (FieldPolynomial.isZero(FieldPolynomial(3, PrimeField(1)))) should be (false)
  }

  it should "say that 0. is 0" in {
    (FieldPolynomial.isZero(FieldPolynomial())) should be (true)
  }

  it should "say that [+ PF(11, 1)x^3, [+ PF(11, 1)x^2, 0.]] > [+ PF(11, 1)x^3, 0.]" in {
    val a = FieldPolynomial(3, PrimeField(1), FieldPolynomial(2, PrimeField(1)))
    val b = FieldPolynomial(3, PrimeField(1))

    (FieldPolynomial.greater(a, b)) should be (true)
    (FieldPolynomial.lesser(b, a)) should be (true)
    (FieldPolynomial.greater(b, a)) should be (false)
    (FieldPolynomial.lesser(a, b)) should be (false)
  }

  it should "say that [+ PF(11, 1)x^3, [+ PF(11, 1)x^2, 0.] is not greater or lesser than itself" in {
    val a = FieldPolynomial(3, PrimeField(1), FieldPolynomial(2, PrimeField(1)))

    (FieldPolynomial.greater(a, a)) should be (false)
    (FieldPolynomial.lesser(a, a)) should be (false)
  }
}
