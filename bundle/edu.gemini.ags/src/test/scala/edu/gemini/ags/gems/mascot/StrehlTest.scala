package edu.gemini.ags.gems.mascot

import edu.gemini.spModel.core.MagnitudeBand
import org.junit.Test
import org.junit.Assert._
import breeze.linalg._
import edu.gemini.ags.gems.mascot.util.YUtils._
import Strehl._

/**
 * Tests methods in the Strehl class.
 */
class StrehlTest {

  //> create_distortion(1,2,3)
  //[1,0]
  //> create_distortion(2,3,4)
  //[0,1]
  //> create_distortion(3,4,5)
  //[0.04,0.05]
  //> create_distortion(4,5,6)
  //[0.0424264,0.0353553]
  //> create_distortion(5,6,7)
  //[0.0424264,-0.0494975]
  //>
  //> x
  //[[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]
  //> y
  //[[1,1,1,1,1],[2,2,2,2,2],[3,3,3,3,3],[4,4,4,4,4],[5,5,5,5,5]]
  //> create_distortion(1,x,y)
  //[[[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1]],[[0,0,0,0,0],[0,
  //0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]]]
  //> create_distortion(2,x,y)
  //[[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],[[1,1,1,1,1],[1,
  //1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1]]]
  //> create_distortion(3,x,y)
  //[[[0.01,0.02,0.03,0.04,0.05],[0.01,0.02,0.03,0.04,0.05],[0.01,0.02,0.03,0.04,
  //0.05],[0.01,0.02,0.03,0.04,0.05],[0.01,0.02,0.03,0.04,0.05]],[[0.01,0.01,0.01,
  //0.01,0.01],[0.02,0.02,0.02,0.02,0.02],[0.03,0.03,0.03,0.03,0.03],[0.04,0.04,
  //0.04,0.04,0.04],[0.05,0.05,0.05,0.05,0.05]]]
  //> create_distortion(4,x,y)
  //[[[0.00707107,0.00707107,0.00707107,0.00707107,0.00707107],[0.0141421,
  //0.0141421,0.0141421,0.0141421,0.0141421],[0.0212132,0.0212132,0.0212132,
  //0.0212132,0.0212132],[0.0282843,0.0282843,0.0282843,0.0282843,0.0282843],
  //[0.0353553,0.0353553,0.0353553,0.0353553,0.0353553]],[[0.00707107,0.0141421,
  //0.0212132,0.0282843,0.0353553],[0.00707107,0.0141421,0.0212132,0.0282843,
  //0.0353553],[0.00707107,0.0141421,0.0212132,0.0282843,0.0353553],[0.00707107,
  //0.0141421,0.0212132,0.0282843,0.0353553],[0.00707107,0.0141421,0.0212132,
  //0.0282843,0.0353553]]]
  //> create_distortion(5,x,y)
  //[[[0.00707107,0.0141421,0.0212132,0.0282843,0.0353553],[0.00707107,0.0141421,
  //0.0212132,0.0282843,0.0353553],[0.00707107,0.0141421,0.0212132,0.0282843,
  //0.0353553],[0.00707107,0.0141421,0.0212132,0.0282843,0.0353553],[0.00707107,
  //0.0141421,0.0212132,0.0282843,0.0353553]],[[-0.00707107,-0.00707107,
  //-0.00707107,-0.00707107,-0.00707107],[-0.0141421,-0.0141421,-0.0141421,
  //-0.0141421,-0.0141421],[-0.0212132,-0.0212132,-0.0212132,-0.0212132,
  //-0.0212132],[-0.0282843,-0.0282843,-0.0282843,-0.0282843,-0.0282843],
  //[-0.0353553,-0.0353553,-0.0353553,-0.0353553,-0.0353553]]]
  @Test def testCreateDistortion() {
    assertVectorsEqual(DenseVector(1.0, 0.0), createDistortion(1, 2.0, 3.0), 0.0001)
    assertVectorsEqual(DenseVector(0.0, 1.0), createDistortion(2, 3.0, 4.0), 0.0001)
    assertVectorsEqual(DenseVector(0.04, 0.05), createDistortion(3, 4.0, 5.0), 0.0001)
    assertVectorsEqual(DenseVector(0.0424264, 0.0353553), createDistortion(4, 5.0, 6.0), 0.0001)
    assertVectorsEqual(DenseVector(0.0424264, -0.0494975), createDistortion(5, 6.0, 7.0), 0.0001)

    val x = DenseMatrix.tabulate(5, 5)((i, j) => i + 1.0)
    val y = x.t

    assertArrayMatricesEqual(
      Array(
        DenseMatrix(
          (1.0, 1.0, 1.0, 1.0, 1.0),
          (1.0, 1.0, 1.0, 1.0, 1.0),
          (1.0, 1.0, 1.0, 1.0, 1.0),
          (1.0, 1.0, 1.0, 1.0, 1.0),
          (1.0, 1.0, 1.0, 1.0, 1.0)),
        DenseMatrix(
          (0.0, 0.0, 0.0, 0.0, 0.0),
          (0.0, 0.0, 0.0, 0.0, 0.0),
          (0.0, 0.0, 0.0, 0.0, 0.0),
          (0.0, 0.0, 0.0, 0.0, 0.0),
          (0.0, 0.0, 0.0, 0.0, 0.0))
      ), createDistortion(1, x, y), 0.001)
    assertArrayMatricesEqual(
      Array(
        DenseMatrix(
          (0.0, 0.0, 0.0, 0.0, 0.0),
          (0.0, 0.0, 0.0, 0.0, 0.0),
          (0.0, 0.0, 0.0, 0.0, 0.0),
          (0.0, 0.0, 0.0, 0.0, 0.0),
          (0.0, 0.0, 0.0, 0.0, 0.0)),
        DenseMatrix(
          (1.0, 1.0, 1.0, 1.0, 1.0),
          (1.0, 1.0, 1.0, 1.0, 1.0),
          (1.0, 1.0, 1.0, 1.0, 1.0),
          (1.0, 1.0, 1.0, 1.0, 1.0),
          (1.0, 1.0, 1.0, 1.0, 1.0))
      ), createDistortion(2, x, y), 0.001)
    assertArrayMatricesEqual(
      Array(
        DenseMatrix(
          (0.01, 0.02, 0.03, 0.04, 0.05),
          (0.01, 0.02, 0.03, 0.04, 0.05),
          (0.01, 0.02, 0.03, 0.04, 0.05),
          (0.01, 0.02, 0.03, 0.04, 0.05),
          (0.01, 0.02, 0.03, 0.04, 0.05)).t,
        DenseMatrix(
          (0.01, 0.01, 0.01, 0.01, 0.01),
          (0.02, 0.02, 0.02, 0.02, 0.02),
          (0.03, 0.03, 0.03, 0.03, 0.03),
          (0.04, 0.04, 0.04, 0.04, 0.04),
          (0.05, 0.05, 0.05, 0.05, 0.05)).t
      ), createDistortion(3, x, y), 0.001)
    assertArrayMatricesEqual(
      Array(
        DenseMatrix(
          (0.00707107, 0.00707107, 0.00707107, 0.00707107, 0.00707107),
          (0.0141421, 0.0141421, 0.0141421, 0.0141421, 0.0141421),
          (0.0212132, 0.0212132, 0.0212132, 0.0212132, 0.0212132),
          (0.0282843, 0.0282843, 0.0282843, 0.0282843, 0.0282843),
          (0.0353553, 0.0353553, 0.0353553, 0.0353553, 0.0353553)).t,
        DenseMatrix(
          (0.00707107, 0.0141421, 0.0212132, 0.0282843, 0.0353553),
          (0.00707107, 0.0141421, 0.0212132, 0.0282843, 0.0353553),
          (0.00707107, 0.0141421, 0.0212132, 0.0282843, 0.0353553),
          (0.00707107, 0.0141421, 0.0212132, 0.0282843, 0.0353553),
          (0.00707107, 0.0141421, 0.0212132, 0.0282843, 0.0353553)).t
      ), createDistortion(4, x, y), 0.001)
    assertArrayMatricesEqual(
      Array(
        DenseMatrix(
          (0.00707107, 0.0141421, 0.0212132, 0.0282843, 0.0353553),
          (0.00707107, 0.0141421, 0.0212132, 0.0282843, 0.0353553),
          (0.00707107, 0.0141421, 0.0212132, 0.0282843, 0.0353553),
          (0.00707107, 0.0141421, 0.0212132, 0.0282843, 0.0353553),
          (0.00707107, 0.0141421, 0.0212132, 0.0282843, 0.0353553)).t,
        DenseMatrix(
          (-0.00707107, -0.00707107, -0.00707107, -0.00707107, -0.00707107),
          (-0.0141421, -0.0141421, -0.0141421, -0.0141421, -0.0141421),
          (-0.0212132, -0.0212132, -0.0212132, -0.0212132, -0.0212132),
          (-0.0282843, -0.0282843, -0.0282843, -0.0282843, -0.0282843),
          (-0.0353553, -0.0353553, -0.0353553, -0.0353553, -0.0353553)).t
      ), createDistortion(5, x, y), 0.001)
  }


  //  > get_distortion_vfield([1,2,3,4,5],5,40,xloc,yloc)
  //    [[[-2.74558,-1.43848,-0.131371,1.17574,2.48284],[-2.1799,-0.872792,0.434315,
  //    1.74142,3.04853],[-1.61421,-0.307107,1,2.30711,3.61421],[-1.04853,0.258579,
  //    1.56569,2.87279,4.1799],[-0.482843,0.824264,2.13137,3.43848,4.74558]],
  //    [[1.08284,1.64853,2.21421,2.7799,3.34558],[0.975736,1.54142,2.10711,2.67279,
  //    3.23848],[0.868629,1.43431,2,2.56569,3.13137],[0.761522,1.32721,1.89289,
  //    2.45858,3.02426],[0.654416,1.2201,1.78579,2.35147,2.91716]]]
  @Test def testGetDistortionVfield() {
    assertArrayMatricesEqual(
      Array(
        DenseMatrix(
          (-2.74558, -1.43848, -0.131371, 1.17574, 2.48284),
          (-2.1799, -0.872792, 0.434315, 1.74142, 3.04853),
          (-1.61421, -0.307107, 1.0, 2.30711, 3.61421),
          (-1.04853, 0.258579, 1.56569, 2.87279, 4.1799),
          (-0.482843, 0.824264, 2.13137, 3.43848, 4.74558)).t,
        DenseMatrix(
          (1.08284, 1.64853, 2.21421, 2.7799, 3.34558),
          (0.975736, 1.54142, 2.10711, 2.67279, 3.23848),
          (0.868629, 1.43431, 2.0, 2.56569, 3.13137),
          (0.761522, 1.32721, 1.89289, 2.45858, 3.02426),
          (0.654416, 1.2201, 1.78579, 2.35147, 2.91716)).t
      ),
      getDistortionVfield(DenseVector(1.0, 2.0, 3.0, 4.0, 5.0), 5, 40.0),
      0.0001)
  }


  //  > get_distortion_vfield([1,2,3,4,5],5,40,xloc,yloc,offset=[3,5])
  //  [[[-3.08307,-1.77597,-0.468858,0.838249,2.14536],[-2.51739,-1.21028,0.0968272,
  //  1.40393,2.71104],[-1.9517,-0.644594,0.662513,1.96962,3.27673],[-1.38602,
  //  -0.0789087,1.2282,2.5353,3.84241],[-0.82033,0.486777,1.79388,3.10099,4.4081]],
  //  [[1.02477,1.59045,2.15614,2.72182,3.28751],[0.91766,1.48335,2.04903,2.61472,
  //  3.1804],[0.810553,1.37624,1.94192,2.50761,3.07329],[0.703446,1.26913,1.83482,
  //  2.4005,2.96619],[0.596339,1.16202,1.72771,2.2934,2.85908]]]
  @Test def testGetDistortionVfieldWithOffset() {
    val offset = DenseVector(3.0, 5.0)
    assertArrayMatricesEqual(
      Array(
        DenseMatrix(
          (-3.08307, -1.77597, -0.468858, 0.838249, 2.14536),
          (-2.51739, -1.21028, 0.0968272, 1.40393, 2.71104),
          (-1.9517, -0.644594, 0.662513, 1.96962, 3.27673),
          (-1.38602, -0.0789087, 1.2282, 2.5353, 3.84241),
          (-0.82033, 0.486777, 1.79388, 3.10099, 4.4081)).t,
        DenseMatrix(
          (1.02477, 1.59045, 2.15614, 2.72182, 3.28751),
          (0.91766, 1.48335, 2.04903, 2.61472, 3.1804),
          (0.810553, 1.37624, 1.94192, 2.50761, 3.07329),
          (0.703446, 1.26913, 1.83482, 2.4005, 2.96619),
          (0.596339, 1.16202, 1.72771, 2.2934, 2.85908)).t
      ),
      getDistortionVfield(DenseVector(1.0, 2.0, 3.0, 4.0, 5.0), 5, 40.0, offset),
      0.0001)
  }

  @Test def testGetStrehlMap() {
    // data taken from test run using MatrixUtil.sFormat()
    val mprop =
      DenseMatrix(
        (3.0262759672544342E-5, 1.3808011158558568E-21, -1.4984697183210646E-5, 3.085052508235402E-5, 3.6650263768179326E-5),
        (1.3808011158558568E-21, 3.0262759672544355E-5, -5.603434475075888E-7, 9.566033272571679E-6, 1.0233358387250115E-4),
        (-1.4984697183210646E-5, -5.603434475075888E-7, 2.38107813404028E-4, -3.8435981955347845E-5, 4.821904065054767E-5),
        (3.085052508235402E-5, 9.566033272571679E-6, -3.8435981955347845E-5, 5.917692383422175E-4, 1.7338950730416165E-4),
        (3.6650263768179326E-5, 1.0233358387250115E-4, 4.821904065054767E-5, 1.7338950730416165E-4, 9.129884895948219E-4))

    val dfields =
      Array(
        Array(
          DenseMatrix(
            (-1.0, -1.0, -1.0, -1.0, -1.0),
            (-1.0, -1.0, -1.0, -1.0, -1.0),
            (-1.0, -1.0, -1.0, -1.0, -1.0),
            (-1.0, -1.0, -1.0, -1.0, -1.0),
            (-1.0, -1.0, -1.0, -1.0, -1.0)),
          DenseMatrix(
            (0.0, 0.0, 0.0, 0.0, 0.0),
            (0.0, 0.0, 0.0, 0.0, 0.0),
            (0.0, 0.0, 0.0, 0.0, 0.0),
            (0.0, 0.0, 0.0, 0.0, 0.0),
            (0.0, 0.0, 0.0, 0.0, 0.0))),
        Array(
          DenseMatrix(
            (1.219307995789658E-16, 1.3851358496864024E-16, 1.5509637035831468E-16, 1.7167915574798912E-16, 1.8826194113766357E-16),
            (4.4382614399808465E-17, 6.09653997894829E-17, 7.754818517915734E-17, 9.413097056883178E-17, 1.1071375595850622E-16),
            (-3.3165570779348877E-17, -1.6582785389674438E-17, 0.0, 1.6582785389674438E-17, 3.3165570779348877E-17),
            (-1.1071375595850622E-16, -9.413097056883178E-17, -7.754818517915734E-17, -6.09653997894829E-17, -4.4382614399808465E-17),
            (-1.8826194113766357E-16, -1.7167915574798912E-16, -1.5509637035831468E-16, -1.3851358496864024E-16, -1.219307995789658E-16)),
          DenseMatrix(
            (-1.0, -1.0, -1.0, -1.0, -0.9999999999999999),
            (-1.0, -1.0, -1.0, -1.0, -0.9999999999999999),
            (-1.0, -1.0, -1.0, -1.0, -0.9999999999999999),
            (-1.0, -1.0, -1.0, -1.0, -0.9999999999999999),
            (-1.0, -1.0, -1.0, -1.0, -0.9999999999999999))),
        Array(
          DenseMatrix(
            (-0.33781410293140834, -0.3911226003223497, -0.44443109771329103, -0.4977395951042324, -0.5510480924951737),
            (-0.11559855407476284, -0.16890705146570417, -0.22221554885664552, -0.27552404624758686, -0.3288325436385282),
            (0.10661699478188269, 0.053308497390941345, 0.0, -0.053308497390941345, -0.10661699478188269),
            (0.3288325436385282, 0.27552404624758686, 0.22221554885664552, 0.16890705146570417, 0.11559855407476284),
            (0.5510480924951737, 0.4977395951042324, 0.44443109771329103, 0.3911226003223497, 0.33781410293140834)),
          DenseMatrix(
            (-0.1278311075048267, -0.01060705636147223, 0.10661699478188226, 0.22384104592523668, 0.3410650970685912),
            (-0.18113960489576805, -0.06391555375241358, 0.053308497390940915, 0.1705325485342954, 0.2877565996776499),
            (-0.2344481022867094, -0.11722405114335492, -4.3204E-16, 0.11722405114335403, 0.2344481022867085),
            (-0.28775659967765077, -0.17053254853429628, -0.053308497390941775, 0.06391555375241269, 0.18113960489576716),
            (-0.3410650970685921, -0.22384104592523757, -0.10661699478188312, 0.010607056361471343, 0.12783110750482582))),
        Array(
          DenseMatrix(
            (0.39998824438565683, 0.3007593840963237, 0.20153052380699055, 0.10230166351765739, 0.0030728032283242335),
            (0.29922298248216156, 0.19999412219282842, 0.10076526190349527, 0.0015364016141621167, -0.09769245867517105),
            (0.1984577205786663, 0.09922886028933316, 0.0, -0.09922886028933316, -0.1984577205786663),
            (0.09769245867517105, -0.0015364016141621167, -0.10076526190349527, -0.19999412219282842, -0.29922298248216156),
            (-0.0030728032283242335, -0.10230166351765739, -0.20153052380699055, -0.3007593840963237, -0.39998824438565683)),
          DenseMatrix(
            (-0.003072803228324039, 0.09769245867517125, 0.19845772057866656, 0.29922298248216184, 0.39998824438565717),
            (-0.10230166351765721, -0.0015364016141619086, 0.09922886028933339, 0.1999941221928287, 0.300759384096324),
            (-0.20153052380699035, -0.10076526190349507, 2.41746E-16, 0.10076526190349555, 0.20153052380699085),
            (-0.3007593840963235, -0.19999412219282822, -0.09922886028933292, 0.0015364016141623943, 0.1023016635176577),
            (-0.39998824438565667, -0.2992229824821614, -0.19845772057866606, -0.09769245867517078, 0.0030728032283245388))),
        Array(
          DenseMatrix(
            (0.21422232696630347, 0.1287130352882642, 0.043203743610224915, -0.042305548067814364, -0.12781483974585367),
            (0.19262045516119103, 0.10711116348315174, 0.021601871805112458, -0.06390741987292684, -0.14941671155096614),
            (0.17101858335607859, 0.08550929167803929, 0.0, -0.08550929167803929, -0.17101858335607859),
            (0.14941671155096614, 0.06390741987292684, -0.021601871805112458, -0.10711116348315174, -0.19262045516119103),
            (0.12781483974585367, 0.042305548067814364, -0.043203743610224915, -0.1287130352882642, -0.21422232696630347)),
          DenseMatrix(
            (0.5510444397458534, 0.36103151155096586, 0.1710185833560783, -0.018994344838809227, -0.20900727303369676),
            (0.46553514806781404, 0.2755222198729265, 0.08550929167803901, -0.10450363651684852, -0.29451656471173604),
            (0.3800258563897748, 0.19001292819488724, -2.80202E-16, -0.1900129281948878, -0.3800258563897754),
            (0.2945165647117355, 0.10450363651684796, -0.08550929167803957, -0.2755222198729271, -0.4655351480678146),
            (0.2090072730336962, 0.01899434483880867, -0.17101858335607886, -0.3610315115509664, -0.5510444397458539))
        )
      )
    val nmodes_cont = 5
    val bg = DenseVector(-0.7, -0.7, -0.7, -0.7, -0.7)

    val (ret, smap, tiperr, tilterr) = getStrehlMap(mprop, dfields, nmodes_cont, bg)

    assertEquals(0.13261345432224386, ret, 0.0000001)
    assertMatricesEqual(DenseMatrix(
      (0.8709863155675973, 0.8754236272254221, 0.8770735599113669, 0.8759027469162969, 0.8719367817759003),
      (0.8751054729667774, 0.8791268699573285, 0.8803329348581569, 0.8786991129334836, 0.874260355885239),
      (0.8767781894704555, 0.8803572716785626, 0.8811085485081857, 0.8790165959643051, 0.8741255477461062),
      (0.8759754040044704, 0.8790928605646466, 0.8793858291942424, 0.8768481242432511, 0.8715326715906324),
      (0.8727122398569872, 0.8753563420955445, 0.8751949607523837, 0.8722310418126616, 0.8665257137577401)),
      smap, 0.0000001
    )
    assertMatricesEqual(DenseMatrix(
      (0.007457392511221423, 0.007362863476305808, 0.007321840881640067, 0.0073352224944056455, 0.007402713287022161),
      (0.0072814870872136735, 0.007196814312500731, 0.007167060632993999, 0.007192907604470555, 0.007273762528219452),
      (0.007214824941012427, 0.007141624991545698, 0.007123935424421412, 0.007162167560440311, 0.007255437424260636),
      (0.007260415776731238, 0.007199845302816041, 0.007194473388580823, 0.007244422828255823, 0.0073485656227015896),
      (0.007416189666258419, 0.0073687876100788615, 0.007375414045328328, 0.007435924532877002, 0.007549023433418312)),
      tiperr, 0.00000001
    )
    assertMatricesEqual(DenseMatrix(
      (0.007470601757762836, 0.007268719732496349, 0.007198680961735571, 0.0072642999484400644, 0.007461998727267263),
      (0.007371451371785786, 0.007185486485388182, 0.0071334747388330805, 0.00721831232215834, 0.007435316323039387),
      (0.007325656274434186, 0.007157281979554232, 0.007123935424421412, 0.007227485845062624, 0.0074622364250012),
      (0.007334215990598617, 0.00718475430749363, 0.007170285198939016, 0.007291612326638006, 0.007542185089389844),
      (0.0073969418250645295, 0.007267272074464113, 0.0072714553947585085, 0.007409265066416446, 0.007673505014245794)),
      tilterr, 0.0000001
    )
  }


  // Test top level Strehl algorithm with data from the Yorick version
  @Test def testStrehl() {
    val starList = List(
      MascotTest.star(1.25168, 0.801961, 11.34, 9.08, 11.09, 12.769, 11.977, 11.298, 49.9505, 41.5119),
      MascotTest.star(-32.9534, 43.4231, 14.19, 13.55, 12.63, 11.678, 11.088, 10.979, 49.9632, 41.5238),
      MascotTest.star(42.4108, 15.864, 14.71, 14.06, 13.11, 12.677, 12.144, 12.063, 49.9352, 41.5161))

    val s = Strehl(starList, Mascot.defaultMagnitudeExtractor)
    val starmag = (for (star <- s.stars) yield star.target.magnitudeIn(MagnitudeBand.R).get.value).toArray
    val starra = (for (star <- starList) yield star.target.coordinates.ra.toAngle.toDegrees).toArray
    val stardec = (for (star <- starList) yield star.target.coordinates.dec.toDegrees).toArray
    val starx = (for (star <- starList) yield star.x).toArray
    val stary = (for (star <- starList) yield star.y).toArray

    assertEquals(0.882734, s.avgstrehl, 0.01)
    assertEquals(0.887949, s.maxstrehl, 0.001)
    assertEquals(40, s.halffield, 0.001)
    assertArrayEquals(Array(1.25168, -32.9534, 42.4108), starx, 0.001)
    assertArrayEquals(Array(0.801961, 43.4231, 15.864), stary, 0.001)
    assertArrayEquals(Array(49.9505, 49.9632, 49.9352), starra, 0.001)
    assertArrayEquals(Array(41.5119, 41.5238, 41.5161), stardec, 0.001)
    assertArrayEquals(Array(11.09, 12.63, 13.11), starmag, 0.001)
    assertEquals(0.00416973, s.rmsstrehl, 0.001)
    assertEquals(0.867047, s.minstrehl, 0.001)
  }
}
