package me.stojan.reunion.polynomials

import me.stojan.reunion.structure.{ Field, FieldDescriptor }
import me.stojan.reunion.structure.concrete.PrimeField
import me.stojan.reunion.euclidean.{ Euclidean, ExtendedEuclidean }

import me.stojan.polynome.Polynomial

class FieldPolynomialAsEuclideanSpec extends ReunionPolynomialsSpec {
  implicit val primeFieldDescriptor: FieldDescriptor[BigInt] = PrimeField.descriptor(11)

  "FieldPolynomial as a Euclidean domain" should "convert to a Euclidean domain representation" in {

    val a = FieldPolynomial(1, PrimeField(1))
    val e: Euclidean[Polynomial[Field[BigInt]]] = PolynomialRing.euclidean(a)(primeFieldDescriptor)

    e.value should be (a)
  }

  it should "run GCD([+ PF(11, 1)x^1, 0.], [+ PF(11, 1)x^1, 0.])" in {
    val a = FieldPolynomial(1, PrimeField(1))
    val e = PolynomialRing.euclidean(a)(primeFieldDescriptor)
    val eea = ExtendedEuclidean(e, e)

    eea.gcd should be (e)
    ((eea.input._1 * eea.bezout._1) + (eea.input._2 * eea.bezout._2)) should be (eea.gcd)
  }

  it should "run GCD([+ PF(2, 1)x^3, [+ PF(2, 1)x^2, 0.]], Rijndael)" in {
    implicit val primeFieldDescriptor = PrimeField.descriptor(2)

    // rijndael is the irreducible polynomial of GF(2^8)
    val rijndael = FieldPolynomial(8, PrimeField(1), FieldPolynomial(4, PrimeField(1), FieldPolynomial(3, PrimeField(1), FieldPolynomial(1, PrimeField(1), FieldPolynomial(0, PrimeField(1))))))
    val a = FieldPolynomial(3, PrimeField(1), FieldPolynomial(2, PrimeField(1)))

    val eea = ExtendedEuclidean(PolynomialRing.euclidean(a), PolynomialRing.euclidean(rijndael))

    eea.gcd should be (PolynomialRing.euclidean(FieldPolynomial(0, PrimeField(1))))
    ((eea.input._1 * eea.bezout._1) + (eea.input._2 * eea.bezout._2)) should be (eea.gcd)

    val eea2 = ExtendedEuclidean(PolynomialRing.euclidean(rijndael), PolynomialRing.euclidean(a))

    eea should be (eea2)
  }
}
