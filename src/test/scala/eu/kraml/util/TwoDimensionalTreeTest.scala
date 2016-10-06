package eu.kraml.util

import org.scalatest.FlatSpec


class TwoDimensionalTreeTest extends FlatSpec {
    "tree builder" should "build tree" in {
        val points = List(
            ((-1, 1), 1),
            ((1, 1), 2),
            ((1, -1), 3),
            ((-1, -1), 4)
        )

        val tree = TwoDimensionalTree.makeTree(points)

        println(s"height ${tree.height}")
        println(s"size ${tree.size}")
    }
}
