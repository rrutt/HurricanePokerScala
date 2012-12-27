package com.live.rrutt.tuprolog.util;

public class MenuDialog extends javax.swing.JDialog {

	private static final long serialVersionUID = 846223950995979794L;

	private int choice = 0;

	private String caption = "";

	/** Creates new form MenuDialog */
	public MenuDialog(java.awt.Frame parent, boolean modal, String caption, java.util.List<String> choices) {
		super(parent, modal);
		this.caption = caption;
		initComponents(choices);
		setLocationRelativeTo(null); // Center on screen.
	}

	/**
	 * This method is called from within the constructor to initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is always
	 * regenerated by the Form Editor.
	 */
	private void initComponents(java.util.List<String> choices) { // GEN-BEGIN:initComponents
		jLabel1 = new javax.swing.JLabel();

		java.awt.GridLayout gl = new java.awt.GridLayout(0, 1);
		getContentPane().setLayout(gl);

		setTitle("Hurricane");
		addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent evt) {
				closeDialog(evt);
			}
		});

		jLabel1.setFont(new java.awt.Font("Arial", 0, 12));
		jLabel1.setText(Utilities.stripQuotes(caption));
		jLabel1.setMinimumSize(new java.awt.Dimension(150, 15));
		jLabel1.setPreferredSize(new java.awt.Dimension(150, 15));
		getContentPane().add(jLabel1);

		javax.swing.JButton btn = null;
		int i = 0;
		@SuppressWarnings("rawtypes")
		java.util.Iterator iter = choices.listIterator();
		while (iter.hasNext()) {
			i++;
			String choiceText = (String)iter.next();
			if (choiceText.length() > 0) {
				btn = new javax.swing.JButton();
				btn.setFont(new java.awt.Font("Arial", 0, 10));
				btn.setName(Integer.toString(i));
				btn.setText(choiceText);
				btn.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(java.awt.event.ActionEvent evt) {
						btnActionPerformed(evt);
					}
				});
				getContentPane().add(btn);
			}
		}

		pack();
	} // GEN-END:initComponents

	private void btnActionPerformed(java.awt.event.ActionEvent evt) { // GEN-FIRST:event_bYesActionPerformed
		javax.swing.JButton btn = (javax.swing.JButton) (evt.getSource());
		choice = Integer.parseInt(btn.getName());
		dispose();
	} // GEN-LAST:event_bYesActionPerformed

	/** Closes the dialog */
	private void closeDialog(java.awt.event.WindowEvent evt) { // GEN-FIRST:event_closeDialog
		setVisible(false);
		dispose();
	} // GEN-LAST:event_closeDialog

	public int choice() {
		return this.choice;
	}

	// Variables declaration - do not modify//GEN-BEGIN:variables
	private javax.swing.JLabel jLabel1;
	// End of variables declaration//GEN-END:variables
}