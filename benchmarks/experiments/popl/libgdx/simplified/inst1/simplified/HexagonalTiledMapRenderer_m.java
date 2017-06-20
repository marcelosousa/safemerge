/*******************************************************************************
 * Copyright 2013 See AUTHORS file.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package com.badlogic.gdx.maps.tiled.renderers;

import static com.badlogic.gdx.graphics.g2d.Batch.*;

import com.badlogic.gdx.graphics.Color;
import com.badlogic.gdx.graphics.g2d.Batch;
import com.badlogic.gdx.graphics.g2d.TextureRegion;
import com.badlogic.gdx.maps.tiled.TiledMap;
import com.badlogic.gdx.maps.tiled.TiledMapTile;
import com.badlogic.gdx.maps.tiled.TiledMapTileLayer;
import com.badlogic.gdx.maps.tiled.tiles.AnimatedTiledMapTile;

public class HexagonalTiledMapRenderer extends BatchTiledMapRenderer {

	/** true for X-Axis, false for Y-Axis */
	private boolean staggerAxisX = true;
	/** true for even StaggerIndex, false for odd */
	private boolean staggerIndexEven = false;
	/** the parameter defining the shape of the hexagon from tiled. more specifically it represents the length of the sides that
	 * are parallel to the stagger axis. e.g. with respect to the stagger axis a value of 0 results in a rhombus shape, while a
	 * value equal to the tile length/height represents a square shape and a value of 0.5 represents a regular hexagon if tile
	 * length equals tile height */
	private float hexSideLength = 0f;
        private Batch batch;

	@Override
	public int renderTileLayer (TiledMapTileLayer layer) {
		 Color batchColor = batch.getColor();
		 float color = Color.toFloatBits(batchColor.r, batchColor.g, batchColor.b, batchColor.a * layer.getOpacity());

		 int layerWidth = layer.getWidth();
		 int layerHeight = layer.getHeight();

		 float layerTileWidth = layer.getTileWidth() * unitScale;
		 float layerTileHeight = layer.getTileHeight() * unitScale;

		 float layerOffsetX = layer.getOffsetX() * unitScale;
		// offset in tiled is y down, so we flip it
		 float layerOffsetY = -layer.getOffsetY() * unitScale;

		 float layerHexLength = hexSideLength * unitScale;

		if (staggerAxisX) {
			 float tileWidthLowerCorner = (layerTileWidth - layerHexLength) / 2;
			 float tileWidthUpperCorner = (layerTileWidth + layerHexLength) / 2;
			 float layerTileHeight50 = layerTileHeight * 0.50f;

			 int row1 = Math.max(0, (int)((viewBounds.y - layerTileHeight50 - layerOffsetX) / layerTileHeight));
			 int row2 = Math.min(layerHeight,
				(int)((viewBounds.y + viewBounds.height + layerTileHeight - layerOffsetX) / layerTileHeight));

			 int col1 = Math.max(0, (int)(((viewBounds.x - tileWidthLowerCorner - layerOffsetY) / tileWidthUpperCorner)));
			 int col2 = Math.min(layerWidth,
				(int)((viewBounds.x + viewBounds.width + tileWidthUpperCorner - layerOffsetY) / tileWidthUpperCorner));

			// depending on the stagger index either draw all even before the odd or vice versa
			 int colA = (staggerIndexEven == (col1 % 2 == 0)) ? col1 + 1 : col1;
			 int colB = (staggerIndexEven == (col1 % 2 == 0)) ? col1 : col1 + 1;

			for (int row = row2 - 1; row >= row1; row--) {
				for (int col = colA; col < col2; col += 2) {
					renderCell(layer.getCell(col, row), tileWidthUpperCorner * col + layerOffsetX,
						layerTileHeight50 + (layerTileHeight * row) + layerOffsetY, color);
				}
				for (int col = colB; col < col2; col += 2) {
					renderCell(layer.getCell(col, row), tileWidthUpperCorner * col + layerOffsetX,
						layerTileHeight * row + layerOffsetY, color);
				}
			}
		} else {
			 float tileHeightLowerCorner = (layerTileHeight - layerHexLength) / 2;
			 float tileHeightUpperCorner = (layerTileHeight + layerHexLength) / 2;
			 float layerTileWidth50 = layerTileWidth * 0.50f;

			 int row1 = Math.max(0, (int)(((viewBounds.y - tileHeightLowerCorner - layerOffsetX) / tileHeightUpperCorner)));
			 int row2 = Math.min(layerHeight,
				(int)((viewBounds.y + viewBounds.height + tileHeightUpperCorner - layerOffsetX) / tileHeightUpperCorner));

			 int col1 = Math.max(0, (int)(((viewBounds.x - layerTileWidth50 - layerOffsetY) / layerTileWidth)));
			 int col2 = Math.min(layerWidth,
				(int)((viewBounds.x + viewBounds.width + layerTileWidth - layerOffsetY) / layerTileWidth));

			float shiftX = 0;
			for (int row = row2 - 1; row >= row1; row--) {
				// depending on the stagger index either shift for even or uneven indexes
				if ((row % 2 == 0) == staggerIndexEven)
					shiftX = layerTileWidth50;
				else
					shiftX = 0;
				for (int col = col1; col < col2; col++) {
					renderCell(layer.getCell(col, row), layerTileWidth * col + shiftX + layerOffsetX,
						tileHeightUpperCorner * row + layerOffsetY, color);
				}
			}
		}
        return 0;
	}
}
