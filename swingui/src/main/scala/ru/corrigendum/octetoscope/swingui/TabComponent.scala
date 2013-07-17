/*
  This file is part of Octetoscope.
  Copyright (C) 2013 Octetoscope contributors (see /AUTHORS.txt)

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

package ru.corrigendum.octetoscope.swingui

import javax.swing._
import java.awt._
import javax.swing.plaf.basic.BasicButtonUI
import resource.managed
import java.awt.event.{ActionEvent, ActionListener}

private object TabComponent {
  def get(title: String, onClick: () => Unit): JComponent = {
    val panel = new JPanel()

    panel.setLayout(new FlowLayout(FlowLayout.LEADING, 0, 1))
    panel.setOpaque(false)

    val label = new JLabel(title)
    label.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 5))
    panel.add(label)

    val closeButton = new JButton {
      override def paintComponent(g: Graphics) {
        super.paintComponents(g)

        for (g2 <- managed(g.create().asInstanceOf[Graphics2D])) {
          g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
          if (getModel.isPressed) g2.translate(1, 1)
          g2.setStroke(new BasicStroke(2))

          g2.setColor(if (getModel.isRollover) Color.MAGENTA else getForeground)

          val delta = 4
          g2.drawLine(delta, delta, getWidth - delta - 1, getHeight - delta - 1)
          g2.drawLine(getWidth - delta - 1, delta, delta, getHeight - delta - 1)
        }
      }
    }
    closeButton.setUI(new BasicButtonUI)
    closeButton.setContentAreaFilled(false)
    closeButton.setFocusable(false)
    closeButton.setBorder(BorderFactory.createLineBorder(closeButton.getForeground))
    closeButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent) { onClick() }
    })

    val buttonHeight = label.getFontMetrics(label.getFont).getAscent + 2
    closeButton.setPreferredSize(new Dimension(buttonHeight, buttonHeight))
    closeButton.setMinimumSize(new Dimension(buttonHeight, buttonHeight))

    panel.add(closeButton)

    panel
  }
}
