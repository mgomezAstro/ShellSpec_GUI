#ifndef PLOTTING_SS_H
#define PLOTTING_SS_H

#include <QDialog>
#include "shellspec.h"

extern int cfort;
extern QString prefix;


namespace Ui {
class plotting_ss;
}

class plotting_ss : public QDialog
{
    Q_OBJECT

public:
    void plott_example();
    void plot_fort_files(QString fortFile);
    void plot_lightcurve_file(QString lightFile);
    void plot_shellspectrum_file(QString shellspecFile, bool overplot);
    void clear_plot();
    int count_total_graphs_current_model();
    explicit plotting_ss(QWidget *parent = nullptr);
    ~plotting_ss();
    shellspec ss;

private slots:
    void on_buttonPlotSave_clicked();

    void on_radioLightcurve_toggled(bool checked);

    void on_radio2Dphases_toggled(bool checked);

    void on_radioShellspectrum_toggled(bool checked);

    void on_buttonOP_clicked();

    void on_buttonPlot_clicked();

    void on_buttonPlotBack_clicked();

    void on_buttonPlotNext_clicked();

private:
    Ui::plotting_ss *ui;
};

#endif // PLOTTING_SS_H
