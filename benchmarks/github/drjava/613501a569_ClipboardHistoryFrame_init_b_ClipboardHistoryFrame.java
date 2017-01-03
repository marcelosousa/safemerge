{
  addComponentListener(new java.awt.event.ComponentAdapter()
                       {
                         public void componentResized (ComponentEvent e)
                         {
                           validate();
                           _historyList.ensureIndexIsVisible(_historyList.getSelectedIndex());
                         }
                       });
  JRootPane rootPane = this.getRootPane();
  InputMap iMap = rootPane.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
  iMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "escape");
  ActionMap aMap = rootPane.getActionMap();
  aMap.put("escape", new AbstractAction()
                     {
                       public void actionPerformed (ActionEvent e)
                       {
                         cancelButtonPressed();
                       }
                     });
  _historyList = new JList<HistoryString>();
  _historyList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
  _historyList.addListSelectionListener(new ListSelectionListener()
                                        {
                                          public void valueChanged (ListSelectionEvent e)
                                          {
                                            updatePreview();
                                          }
                                        });
  _historyList.setFont(DrJava.getConfig().getSetting(OptionConstants.FONT_MAIN));
  _historyList.setCellRenderer(new DefaultListCellRenderer()
                               {
                                 public Component getListCellRendererComponent (JList list, Object value, int index, boolean isSelected, boolean cellHasFocus)
                                 {
                                   Component c = super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
                                   c.setForeground(DrJava.getConfig().getSetting(OptionConstants.DEFINITIONS_NORMAL_COLOR));
                                   return c;
                                 }
                               });
  _historyList.addFocusListener(new FocusAdapter()
                                {
                                  public void focusLost (FocusEvent e)
                                  {
                                    if (e.getOppositeComponent() != _previewArea && e.getOppositeComponent() != _okButton && e.getOppositeComponent() != _cancelButton)
                                    {
                                      _historyList.requestFocus();
                                    }
                                  }
                                });
  _okButton = new JButton("OK");
  _okButton.addActionListener(new ActionListener()
                              {
                                public void actionPerformed (ActionEvent e)
                                {
                                  okButtonPressed();
                                }
                              });
  _cancelButton = new JButton("Cancel");
  _cancelButton.addActionListener(new ActionListener()
                                  {
                                    public void actionPerformed (ActionEvent e)
                                    {
                                      cancelButtonPressed();
                                    }
                                  });
  Container contentPane = getContentPane();
  GridBagLayout layout = new GridBagLayout();
  contentPane.setLayout(layout);
  GridBagConstraints c = new GridBagConstraints();
  c.anchor = GridBagConstraints.NORTHWEST;
  c.weightx = 1.0;
  c.weighty = 0.0;
  c.gridwidth = GridBagConstraints.REMAINDER;
  c.insets.top = 2;
  c.insets.left = 2;
  c.insets.bottom = 2;
  c.insets.right = 2;
  c.fill = GridBagConstraints.BOTH;
  c.weighty = 1.0;
  contentPane.add(new JScrollPane(_historyList, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED), c);
  _previewArea = new JTextArea("");
  _previewArea.setEditable(false);
  _previewArea.setDragEnabled(false);
  _previewArea.setEnabled(false);
  _previewArea.setFont(DrJava.getConfig().getSetting(OptionConstants.FONT_MAIN));
  _previewArea.setDisabledTextColor(DrJava.getConfig().getSetting(OptionConstants.DEFINITIONS_NORMAL_COLOR));
  c.weighty = 2.0;
  contentPane.add(new JScrollPane(_previewArea, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED), c);
  c.anchor = GridBagConstraints.SOUTH;
  JPanel buttonPanel = new JPanel(new GridBagLayout());
  GridBagConstraints bc = new GridBagConstraints();
  bc.insets.left = 2;
  bc.insets.right = 2;
  buttonPanel.add(_okButton, bc);
  buttonPanel.add(_cancelButton, bc);
  c.weighty = 0.0;
  contentPane.add(buttonPanel, c);
  Dimension parentDim = _mainFrame != null ? _mainFrame.getSize() : getToolkit().getScreenSize();
  int xs = (int) parentDim.getWidth() / 3;
  int ys = (int) parentDim.getHeight() / 4;
  setSize(Math.max(xs, 400), Math.max(ys, 300));
  Utilities.setPopupLoc(this, _mainFrame);
  updateView();
}