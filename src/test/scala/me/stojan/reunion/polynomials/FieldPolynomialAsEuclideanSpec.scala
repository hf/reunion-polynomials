package me.stojan.reunion.polynomials

import me.stojan.reunion.structure.Field
import me.stojan.reunion.structure.concrete.PrimeField
import me.stojan.reunion.euclidean.{ Euclidean, ExtendedEuclidean }

import me.stojan.polynome.Polynomial

class FieldPolynomialAsEuclideanSpec extends ReunionPolynomialsSpec {
  "FieldPolynomial as a Euclidean domain" should "convert to a Euclidean domain representation" in {
    val a = FieldPolynomial(1, PrimeField(11, 1))
    val e: Euclidean[Polynomial[Field[BigInt]]] = PolynomialRing.euclidean(a)

    e.value should be (a)
  }

  it should "run GCD([+ PF(11, 1)x^1, 0.], [+ PF(11, 1)x^1, 0.])" in {
    val a = FieldPolynomial(1, PrimeField(11, 1))
    val e = PolynomialRing.euclidean(a)
    val eea = ExtendedEuclidean(e, e)

    eea.gcd should be (e)
    ((eea.input._1 * eea.bezout._1) + (eea.input._2 * eea.bezout._2)) should be (eea.gcd)
  }

  it should "run GCD([+ PF(2, 1)x^3, [+ PF(2, 1)x^2, 0.]], Rijndael)" in {
    // rijndael is the irreducible polynomial of GF(2^8)
    val rijndael = FieldPolynomial(8, PrimeField(2, 1), FieldPolynomial(4, PrimeField(2, 1), FieldPolynomial(3, PrimeField(2, 1), FieldPolynomial(1, PrimeField(2, 1), FieldPolynomial(0, PrimeField(2, 1))))))
    val a = FieldPolynomial(3, PrimeField(2, 1), FieldPolynomial(2, PrimeField(2, 1)))

    val eea = ExtendedEuclidean(PolynomialRing.euclidean(a), PolynomialRing.euclidean(rijndael))

    eea.gcd should be (PolynomialRing.euclidean(FieldPolynomial(0, PrimeField(2, 1))))
    ((eea.input._1 * eea.bezout._1) + (eea.input._2 * eea.bezout._2)) should be (eea.gcd)

    val eea2 = ExtendedEuclidean(PolynomialRing.euclidean(rijndael), PolynomialRing.euclidean(a))

    eea should be (eea2)
  }
}
