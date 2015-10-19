package me.stojan.reunion.polynomials

import me.stojan.reunion.structure.{ Field, Ring, FieldDescriptor }
import me.stojan.reunion.euclidean.Euclidean

import me.stojan.polynome.Polynomial

/**
 * Implicit conversions to polynomial rings.
 */
object PolynomialRingConversions {
  /**
   * Implicitly converts a polynomial with field coefficients to a euclidean domain.
   */
  implicit def fieldPolynomialToEuclidean[V](p: Polynomial[Field[V]])(implicit coefficientDescriptor: FieldDescriptor[V]): Euclidean[Polynomial[Field[V]]] = PolynomialRing.euclidean(p)

  /**
   * Implicitly converts a polynomial with field coefficients to a ring.
   */
  implicit def fieldPolynomialToRing[V](p: Polynomial[Field[V]])(implicit coefficientDescriptor: FieldDescriptor[V]): Ring[Polynomial[Field[V]]] = PolynomialRing.ring(p)

  /**
   * Implicitly converts a polynomial ring (exposed as a ring) to the polynomial representation.
   */
  implicit def ringFPToFieldPolynomial[V](r: Ring[Polynomial[Field[V]]]): Polynomial[Field[V]] = r.value

  /**
   * Implicitly converts a polynomial euclidean domain to the polynomial representation.
   */
  implicit def euclideanFPToFieldPolynomial[V](e: Euclidean[Polynomial[Field[V]]]): Polynomial[Field[V]] = e.value
}
