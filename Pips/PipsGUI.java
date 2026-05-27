import org.jpl7.Query;
import org.jpl7.Term;
import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.List;

/**
 * Assignatura : Llenguatges de Programació, curs 2025-26
 * Pràctica    : Pràctica final – PROLOG (Pips puzzle)
 * 
 * @author Carolina Marín Sánchez, Nour Iman Mehannek Samah
 *
 * Data: 01/06/2026
 * 
 * Grup        : 101
 * Professors  : 
 *      - Cabot Nadal, Miquel Àngel
 *      - Oliver Tomàs, Antoni
 * 
 * PipsGUI.java
 * Interfície gràfica per al puzle Pips, integrada amb SWI-Prolog via JPL.
 * 
 * Funcionalitats:
 *  - Seleccionar puzle per ID i dificultat (easy, medium)
 *  - Mostrar el tauler buit amb les regions acolorides
 *  - Col·locar peces de dominó al tauler manualment
 *  - Resoldre automàticament amb Prolog
 *  - Comprovar si la solució manual és correcta
 *
 * Requisits:
 *  - SWI-Prolog instal·lat (https://www.swi-prolog.org/)
 *  - JPL (jpl.jar) inclòs amb la instal·lació de SWI-Prolog
 *  - pips.pl i puzzles.pl al mateix directori que PipsGUI.java
 *
 * ============================================================
 * COMPILACIÓ I EXECUCIÓ
 * ============================================================
 *
 * WINDOWS
 * -------
 * 1. Obre una terminal (PowerShell o CMD) al directori dels fitxers.
 *
 * 2. Compila:
 *      javac "-cp" ".;C:\Program Files\swipl\lib\jpl.jar" PipsGUI.java
 *
 * 3. Executa:
 *      java "-cp" ".;C:\Program Files\swipl\lib\jpl.jar" "-Djava.library.path=C:\Program Files\swipl\bin" PipsGUI
 *
 * LINUX
 * -----
 * 1. Obre una terminal al directori dels fitxers.
 *
 * 2. Compila:
 *      javac -cp ".:/usr/lib/swi-prolog/lib/jpl.jar" PipsGUI.java
 *
 * 3. Executa:
 *      java -cp ".:/usr/lib/swi-prolog/lib/jpl.jar" \
 *           -Djava.library.path=/usr/lib/swi-prolog/lib/amd64 \
 *           PipsGUI
 *
 * macOS
 * -----
 * 1. Obre una terminal al directori dels fitxers.
 *
 * 2. Compila:
 *      javac -cp ".:/usr/local/lib/swipl/lib/jpl.jar" PipsGUI.java
 *
 * 3. Executa:
 *      java -cp ".:/usr/local/lib/swipl/lib/jpl.jar" \
 *           -Djava.library.path=/usr/local/lib/swipl/lib \
 *           PipsGUI
 * ============================================================
 */
public class PipsGUI extends JFrame {

    // Colors per a les regions del tauler
    private static final Color[] REGION_COLORS = {
        new Color(255, 220, 120),
        new Color(150, 210, 255),
        new Color(180, 255, 180),
        new Color(255, 180, 180),
        new Color(200, 160, 255),
        new Color(255, 200, 140),
        new Color(160, 240, 220),
        new Color(255, 160, 200),
        new Color(200, 230, 255),
        new Color(230, 255, 160),
        new Color(255, 230, 200),
        new Color(200, 200, 200),
    };
    private static final Color COLOR_SELECTED = new Color(100, 180, 255); // casella seleccionada
    private static final Color COLOR_PLACED   = new Color(80,  160, 80);  // peça ja col·locada
    private static final Color COLOR_ERROR    = new Color(255, 100, 100); // error / incorrecte
    private static final Color COLOR_BG       = new Color(40,  40,  50);  // fons general

    // Estat del puzle actual
    private List<int[][]> regions;           // llista de caselles per cada regió
    private List<String>  regionConds;       // condició de cada regió (sum, equals, ...)
    private List<java.lang.Integer> regionObjs; // objectiu numèric de cada regió (-1 si nil)
    private List<int[]>   pieces;            // peces de dominó [[v1,v2], ...]
    private int[][]       board;             // valor de cada casella (-1 = buida)
    private int[][]       boardRegion;       // índex de regió de cada casella (-1 = fora)
    private int           rows, cols;        // dimensions del tauler
    private int           selectedPiece = -1; // índex de la peça seleccionada (-1 = cap)
    private int[]         firstCell     = null; // primera meitat pendent de col·locar

    // Posicions de les peces col·locades manualment: placedPositions[i] = [[r1,c1],[r2,c2]]
    private int[][][] placedPositions;

    // Components de la interfície
    private JPanel     boardPanel;
    private JPanel     piecesPanel;
    private JLabel[][] cellLabels;
    private JLabel[]   pieceLabels;
    private JLabel     statusLabel;
    private JComboBox<String> puzzleCombo;
    private JComboBox<String> diffCombo;

    // IDs de puzles disponibles a la base de coneixements
    private static final String[] PUZZLE_IDS   = {
        "20250818","20250819","20250820","20250821","20250822","20250823","20250824"
    };
    // Només easy i medium (hard no està implementat)
    private static final String[] DIFFICULTIES = {"easy","medium"};

    // Constructor: inicialitza Prolog i construeix la UI
    public PipsGUI() {
        super("Pips Puzzle");
        initProlog();
        buildUI();
        SwingUtilities.invokeLater(() -> {
            try {
                Thread.sleep(500);
                loadPuzzle("20250818", "easy");
            } catch (InterruptedException e) {
                setStatus("Error carregant puzzle inicial", true);
            }
        });
    }

    /**
     * Inicialitza la connexió amb SWI-Prolog i carrega pips.pl.
     * Mostra un diàleg d'error si no es troba el fitxer.
     */
    private void initProlog() {
        try {
            String path = System.getProperty("user.dir").replace("\\", "/");
            String pipsFile = path + "/pips.pl";
            Query q = new Query("consult('" + pipsFile + "')");
            if (!q.hasSolution()) {
                throw new Exception("consult falló para " + pipsFile);
            }
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,
                "Error inicialitzant Prolog:\n" + e.getMessage() +
                "\n\nVerifica que pips.pl i puzzles.pl estan en:\n" +
                System.getProperty("user.dir"),
                "Error", JOptionPane.ERROR_MESSAGE);
        }
    }

    /**
     * Construeix tota la interfície: barra superior, tauler central i panell de peces.
     * L'estat es mostra com a text petit a la barra superior.
     */
    private void buildUI() {
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setBackground(COLOR_BG);
        getContentPane().setBackground(COLOR_BG);
        setLayout(new BorderLayout(10, 10));

        // Barra superior amb controls i etiqueta d'estat
        JPanel topPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 12, 10));
        topPanel.setBackground(new Color(30, 30, 40));
        topPanel.setBorder(BorderFactory.createMatteBorder(0, 0, 2, 0, Color.GRAY));

        puzzleCombo = new JComboBox<>(PUZZLE_IDS);
        diffCombo   = new JComboBox<>(DIFFICULTIES);
        styleCombo(puzzleCombo);
        styleCombo(diffCombo);
        puzzleCombo.setPreferredSize(new Dimension(100, 30));
        diffCombo.setPreferredSize(new Dimension(90, 30));

        JButton loadBtn  = makeButton("Carregar",  new Color(70, 130, 180));
        JButton solveBtn = makeButton("Resoldre",  new Color(60, 160, 80));
        JButton checkBtn = makeButton("Comprovar", new Color(200, 140, 30));
        JButton resetBtn = makeButton("Reset",     new Color(160, 60, 60));

        // Etiqueta d'estat: text petit a la dreta dels botons
        statusLabel = new JLabel("");
        statusLabel.setForeground(Color.LIGHT_GRAY);
        statusLabel.setFont(new Font("SansSerif", Font.ITALIC, 12));
        statusLabel.setBorder(BorderFactory.createEmptyBorder(0, 15, 0, 10));

        loadBtn.addActionListener(e -> loadPuzzle(
            (String) puzzleCombo.getSelectedItem(),
            (String) diffCombo.getSelectedItem()));
        solveBtn.addActionListener(e -> solveWithProlog());
        checkBtn.addActionListener(e -> checkSolution());
        resetBtn.addActionListener(e -> resetBoard());

        topPanel.add(new JLabel(styledLabel("Puzle:")));
        topPanel.add(puzzleCombo);
        topPanel.add(new JLabel(styledLabel("Dificultat:")));
        topPanel.add(diffCombo);
        topPanel.add(loadBtn);
        topPanel.add(solveBtn);
        topPanel.add(checkBtn);
        topPanel.add(resetBtn);
        topPanel.add(statusLabel);
        add(topPanel, BorderLayout.NORTH);

        // Tauler central amb scroll
        boardPanel = new JPanel();
        boardPanel.setBackground(COLOR_BG);
        JScrollPane boardScroll = new JScrollPane(boardPanel);
        boardScroll.setBackground(COLOR_BG);
        boardScroll.setBorder(BorderFactory.createEmptyBorder());
        add(boardScroll, BorderLayout.CENTER);

        // Panell inferior amb les peces disponibles
        piecesPanel = new JPanel();
        piecesPanel.setBackground(new Color(30, 30, 40));
        piecesPanel.setBorder(BorderFactory.createTitledBorder(
            BorderFactory.createLineBorder(Color.GRAY, 2),
            " Peces disponibles ",
            TitledBorder.LEFT, TitledBorder.TOP,
            new Font("SansSerif", Font.BOLD, 13), Color.LIGHT_GRAY));
        JScrollPane piecesScroll = new JScrollPane(piecesPanel);
        piecesScroll.setPreferredSize(new Dimension(0, 130));
        piecesScroll.setBackground(new Color(30, 30, 40));
        piecesScroll.setBorder(BorderFactory.createEmptyBorder());
        add(piecesScroll, BorderLayout.SOUTH);

        setSize(900, 620);
        setLocationRelativeTo(null);
        setVisible(true);
    }

    /**
     * Carrega un puzle de la base de coneixements Prolog i actualitza la UI.
     * @param id   identificador del puzle (p.ex. "20250818")
     * @param diff dificultat: "easy" o "medium"
     */
    private void loadPuzzle(String id, String diff) {
        try {
            String query = String.format("puzzle(%s, %s, Regions, Peces, _)", id, diff);
            Query q = new Query(query);
            if (!q.hasSolution()) {
                setStatus("Puzle no trobat: " + id + " (" + diff + ")", true);
                return;
            }
            Map<String, Term> sol = q.oneSolution();
            Term regionsT = sol.get("Regions");
            Term pecesT = sol.get("Peces");

            if (regionsT == null || pecesT == null) {
                setStatus("Error carregant dades del puzle", true);
                return;
            }

            parseRegions(regionsT);
            parsePieces(pecesT);
            buildBoardFromRegions();
            renderBoard();
            renderPieces();
            selectedPiece = -1;
            firstCell = null;
            setStatus(id + " (" + diff + ") — " + pieces.size() + " peces", false);
        } catch (Exception e) {
            setStatus("Error: " + e.getMessage(), true);
        }
    }

    /**
     * Parseja el terme Prolog de regions i omple les llistes regions, regionConds i regionObjs.
     * @param regionsTerm terme Prolog que representa la llista de regions
     */
    private void parseRegions(Term regionsTerm) {
        regions     = new ArrayList<>();
        regionConds = new ArrayList<>();
        regionObjs  = new ArrayList<>();

        if (regionsTerm.isList()) {
            for (Term reg : regionsTerm.toTermArray()) {
                String cond = reg.arg(1).toString();
                Term   objT = reg.arg(2);
                int    obj  = objT.toString().equals("nil") ? -1
                            : java.lang.Integer.parseInt(objT.toString());
                Term cellsT = reg.arg(3);

                List<int[]> cells = new ArrayList<>();
                if (cellsT.isList()) {
                    for (Term cellTerm : cellsT.toTermArray()) {
                        if (cellTerm.isList()) {
                            Term[] coords = cellTerm.toTermArray();
                            if (coords.length >= 2) {
                                int r = java.lang.Integer.parseInt(coords[0].toString());
                                int c = java.lang.Integer.parseInt(coords[1].toString());
                                cells.add(new int[]{r, c});
                            }
                        }
                    }
                }
                regions.add(cells.toArray(new int[0][]));
                regionConds.add(cond);
                regionObjs.add(obj);
            }
        }
    }

    /**
     * Parseja el terme Prolog de peces i omple la llista pieces.
     * @param piecesTerm terme Prolog que representa la llista de peces
     */
    private void parsePieces(Term piecesTerm) {
        pieces = new ArrayList<>();
        if (piecesTerm.isList()) {
            for (Term piece : piecesTerm.toTermArray()) {
                Term[] vals = piece.toTermArray();
                if (vals.length >= 2) {
                    int v1 = java.lang.Integer.parseInt(vals[0].toString());
                    int v2 = java.lang.Integer.parseInt(vals[1].toString());
                    pieces.add(new int[]{v1, v2});
                }
            }
        }
    }

    /**
     * Inicialitza les estructures board i boardRegion a partir de les regions carregades.
     * Calcula les dimensions del tauler i assigna l'índex de regió a cada casella.
     */
    private void buildBoardFromRegions() {
        rows = 0; cols = 0;
        for (int[][] reg : regions)
            for (int[] cell : reg) {
                rows = Math.max(rows, cell[0] + 1);
                cols = Math.max(cols, cell[1] + 1);
            }
        board        = new int[rows][cols];
        boardRegion  = new int[rows][cols];
        placedPositions = new int[pieces.size()][][];

        for (int[] row : board)       Arrays.fill(row, -1);
        for (int[] row : boardRegion) Arrays.fill(row, -1);

        for (int i = 0; i < regions.size(); i++)
            for (int[] cell : regions.get(i))
                boardRegion[cell[0]][cell[1]] = i;
    }

    /**
     * Renderitza el tauler de joc: crea una JLabel per cada casella amb el color
     * de la seva regió i afegeix listeners per a la interacció manual.
     * Inclou una llegenda de regions a la dreta.
     */
    private void renderBoard() {
        boardPanel.removeAll();

        if (rows == 0 || cols == 0) {
            JLabel errorLabel = new JLabel("No s'han carregat les regions del puzle.");
            errorLabel.setForeground(new Color(255, 100, 100));
            boardPanel.add(errorLabel);
            boardPanel.revalidate();
            boardPanel.repaint();
            return;
        }

        int cellSize = Math.max(70, Math.min(100, 550 / Math.max(rows, cols)));
        boardPanel.setLayout(new GridBagLayout());
        boardPanel.setBackground(COLOR_BG);

        JPanel grid = new JPanel(new GridLayout(rows, cols, 4, 4));
        grid.setBackground(COLOR_BG);
        cellLabels = new JLabel[rows][cols];

        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                JLabel lbl = new JLabel("", SwingConstants.CENTER);
                lbl.setPreferredSize(new Dimension(cellSize, cellSize));
                lbl.setFont(new Font("SansSerif", Font.BOLD, cellSize / 3));
                lbl.setOpaque(true);
                lbl.setBorder(BorderFactory.createLineBorder(new Color(40, 40, 50), 2));

                int ri = boardRegion[r][c];
                if (ri >= 0) {
                    lbl.setBackground(REGION_COLORS[ri % REGION_COLORS.length]);
                    lbl.setForeground(new Color(30, 30, 30));
                    // Mostra l'etiqueta de condició a la primera casella de cada regió
                    if (regions.get(ri)[0][0] == r && regions.get(ri)[0][1] == c) {
                        String tag = condTag(regionConds.get(ri), regionObjs.get(ri));
                        if (!tag.isEmpty()) {
                            lbl.setToolTipText(tag);
                            lbl.setText("<html><div style='font-size:10pt;color:#222;font-weight:bold'>"
                                + tag + "</div></html>");
                        }
                    }
                } else {
                    lbl.setBackground(new Color(20, 20, 30)); // casella fora del tauler
                }

                // Si ja hi ha un valor, el mostra
                if (board[r][c] >= 0) {
                    lbl.setText("<html><b style='font-size:18pt;color:white'>"
                        + board[r][c] + "</b></html>");
                    lbl.setBackground(lbl.getBackground().darker());
                }

                final int fr = r, fc = c;
                lbl.addMouseListener(new MouseAdapter() {
                    public void mouseClicked(MouseEvent e) { onCellClick(fr, fc); }
                    public void mouseEntered(MouseEvent e) {
                        if (boardRegion[fr][fc] >= 0 && selectedPiece >= 0 && board[fr][fc] < 0)
                            lbl.setBorder(BorderFactory.createLineBorder(COLOR_SELECTED, 3));
                    }
                    public void mouseExited(MouseEvent e) {
                        lbl.setBorder(BorderFactory.createLineBorder(new Color(40, 40, 50), 2));
                    }
                });
                cellLabels[r][c] = lbl;
                grid.add(lbl);
            }
        }

        JPanel legend = buildLegend();
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0; gbc.gridy = 0; gbc.insets = new Insets(15, 15, 15, 15);
        boardPanel.add(grid, gbc);
        gbc.gridx = 1; gbc.anchor = GridBagConstraints.NORTH;
        boardPanel.add(legend, gbc);

        boardPanel.revalidate();
        boardPanel.repaint();
    }

    /**
     * Construeix el panell de llegenda amb el color i etiqueta de cada regió.
     * @return JPanel amb la llegenda de regions
     */
    private JPanel buildLegend() {
        JPanel p = new JPanel();
        p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));
        p.setBackground(new Color(30, 30, 40));
        p.setBorder(BorderFactory.createTitledBorder(
            BorderFactory.createLineBorder(Color.GRAY, 2),
            " Regions ", TitledBorder.LEFT, TitledBorder.TOP,
            new Font("SansSerif", Font.BOLD, 12), Color.LIGHT_GRAY));

        for (int i = 0; i < regions.size(); i++) {
            JPanel row = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 4));
            row.setBackground(new Color(30, 30, 40));
            JLabel color = new JLabel("  ");
            color.setOpaque(true);
            color.setPreferredSize(new Dimension(18, 18));
            color.setBackground(REGION_COLORS[i % REGION_COLORS.length]);
            color.setBorder(BorderFactory.createLineBorder(Color.DARK_GRAY, 1));
            String cond = regionConds.get(i);
            int    obj  = regionObjs.get(i);
            String tag  = condTag(cond, obj).isEmpty() ? cond : condTag(cond, obj);
            JLabel txt  = new JLabel(tag);
            txt.setForeground(Color.LIGHT_GRAY);
            txt.setFont(new Font("SansSerif", Font.PLAIN, 11));
            row.add(color); row.add(txt);
            p.add(row);
            if (i < regions.size() - 1) p.add(Box.createVerticalStrut(2));
        }
        p.setMaximumSize(new Dimension(150, p.getPreferredSize().height));
        return p;
    }

    /**
     * Renderitza el panell de peces. Les peces ja col·locades es mostren amb
     * vora verda. Clicar una peça col·locada la retira; clicar una lliure la selecciona.
     */
    private void renderPieces() {
        piecesPanel.removeAll();
        pieceLabels = new JLabel[pieces.size()];
        piecesPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 15, 10));

        for (int i = 0; i < pieces.size(); i++) {
            final int idx = i;
            int[] piece = pieces.get(i);

            JPanel piecePanel = new JPanel(new GridLayout(1, 2, 3, 3));
            piecePanel.setBackground(new Color(30, 30, 40));
            piecePanel.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createLineBorder(Color.GRAY, 2),
                BorderFactory.createEmptyBorder(6, 6, 6, 6)));
            piecePanel.setPreferredSize(new Dimension(100, 50));

            JLabel l1 = makePipLabel(piece[0]);
            JLabel l2 = makePipLabel(piece[1]);
            piecePanel.add(l1);
            piecePanel.add(l2);

            // Vora verda si la peça ja està col·locada
            if (placedPositions[i] != null) {
                piecePanel.setBackground(new Color(40, 80, 40));
                piecePanel.setBorder(BorderFactory.createCompoundBorder(
                    BorderFactory.createLineBorder(COLOR_PLACED, 3),
                    BorderFactory.createEmptyBorder(6, 6, 6, 6)));
            }

            piecePanel.addMouseListener(new MouseAdapter() {
                public void mouseClicked(MouseEvent e) {
                    if (placedPositions[idx] != null) removePiece(idx);
                    else selectPiece(idx);
                }
                public void mouseEntered(MouseEvent e) {
                    if (placedPositions[idx] == null)
                        piecePanel.setBorder(BorderFactory.createCompoundBorder(
                            BorderFactory.createLineBorder(COLOR_SELECTED, 3),
                            BorderFactory.createEmptyBorder(6, 6, 6, 6)));
                }
                public void mouseExited(MouseEvent e) {
                    Color border = placedPositions[idx] != null ? COLOR_PLACED : Color.GRAY;
                    piecePanel.setBorder(BorderFactory.createCompoundBorder(
                        BorderFactory.createLineBorder(border, placedPositions[idx] != null ? 3 : 2),
                        BorderFactory.createEmptyBorder(6, 6, 6, 6)));
                }
            });

            JLabel numLabel = new JLabel("#" + (i+1), SwingConstants.CENTER);
            numLabel.setForeground(Color.GRAY);
            numLabel.setFont(new Font("SansSerif", Font.PLAIN, 11));

            JPanel wrapper = new JPanel(new BorderLayout(0, 3));
            wrapper.setBackground(new Color(30, 30, 40));
            wrapper.add(piecePanel, BorderLayout.CENTER);
            wrapper.add(numLabel, BorderLayout.SOUTH);
            pieceLabels[i] = l1;
            piecesPanel.add(wrapper);
        }

        piecesPanel.revalidate();
        piecesPanel.repaint();
    }

    /**
     * Crea una etiqueta visual per a un valor de pip.
     * @param val valor del pip (0-6)
     * @return JLabel estilitzada
     */
    private JLabel makePipLabel(int val) {
        JLabel l = new JLabel(String.valueOf(val), SwingConstants.CENTER);
        l.setFont(new Font("SansSerif", Font.BOLD, 24));
        l.setForeground(Color.WHITE);
        l.setOpaque(true);
        l.setBackground(new Color(80, 80, 120));
        l.setPreferredSize(new Dimension(45, 40));
        l.setBorder(BorderFactory.createLineBorder(new Color(120, 120, 150), 2));
        return l;
    }

    /**
     * Selecciona una peça per col·locar al tauler.
     * @param idx índex de la peça a seleccionar
     */
    private void selectPiece(int idx) {
        selectedPiece = idx;
        firstCell = null;
        int[] p = pieces.get(idx);
        setStatus("Peça #" + (idx+1) + " [" + p[0] + "|" + p[1] + "] seleccionada", false);
        renderPieces();
    }

    /**
     * Gestiona el clic a una casella del tauler.
     * El primer clic defineix la primera meitat de la peça;
     * el segon clic (casella adjacent) completa la col·locació.
     * @param r fila de la casella clicada
     * @param c columna de la casella clicada
     */
    private void onCellClick(int r, int c) {
        if (selectedPiece < 0) { setStatus("Selecciona una peça primer", false); return; }
        if (boardRegion[r][c] < 0) { setStatus("Casella fora del tauler", true); return; }
        if (board[r][c] >= 0)      { setStatus("Casella ja ocupada", true); return; }

        if (firstCell == null) {
            firstCell = new int[]{r, c};
            cellLabels[r][c].setBackground(COLOR_SELECTED);
            cellLabels[r][c].setText("<html><b style='color:white'>?</b></html>");
            setStatus("Primera meitat a [" + r + "," + c + "] — clica adjacent", false);
        } else {
            if (!isAdjacent(firstCell, new int[]{r, c})) {
                setStatus("Les caselles no son adjacents!", true);
                int ri = boardRegion[firstCell[0]][firstCell[1]];
                cellLabels[firstCell[0]][firstCell[1]]
                    .setBackground(REGION_COLORS[ri % REGION_COLORS.length]);
                cellLabels[firstCell[0]][firstCell[1]].setText("");
                firstCell = null;
                return;
            }
            placePiece(selectedPiece, firstCell, new int[]{r, c});
            firstCell = null;
            selectedPiece = -1;
        }
    }

    /**
     * Col·loca una peça al tauler a les posicions indicades i actualitza la UI.
     * @param pieceIdx índex de la peça
     * @param cell1    coordenades [r,c] de la primera meitat
     * @param cell2    coordenades [r,c] de la segona meitat
     */
    private void placePiece(int pieceIdx, int[] cell1, int[] cell2) {
        int[] p = pieces.get(pieceIdx);
        board[cell1[0]][cell1[1]] = p[0];
        board[cell2[0]][cell2[1]] = p[1];
        placedPositions[pieceIdx] = new int[][]{cell1, cell2};

        updateCell(cell1[0], cell1[1], p[0], false);
        updateCell(cell2[0], cell2[1], p[1], false);
        renderPieces();

        boolean all = true;
        for (int[][] pos : placedPositions) if (pos == null) { all = false; break; }
        setStatus(all ? "Totes les peces col·locades! Prem Comprovar."
                      : "Peça #" + (pieceIdx+1) + " col·locada", false);
    }

    /**
     * Retira una peça del tauler i la torna a l'estat de disponible.
     * @param pieceIdx índex de la peça a retirar
     */
    private void removePiece(int pieceIdx) {
        if (placedPositions[pieceIdx] == null) return;
        int[][] pos = placedPositions[pieceIdx];
        board[pos[0][0]][pos[0][1]] = -1;
        board[pos[1][0]][pos[1][1]] = -1;
        int ri0 = boardRegion[pos[0][0]][pos[0][1]];
        int ri1 = boardRegion[pos[1][0]][pos[1][1]];
        cellLabels[pos[0][0]][pos[0][1]].setBackground(REGION_COLORS[ri0 % REGION_COLORS.length]);
        cellLabels[pos[0][0]][pos[0][1]].setText("");
        cellLabels[pos[1][0]][pos[1][1]].setBackground(REGION_COLORS[ri1 % REGION_COLORS.length]);
        cellLabels[pos[1][0]][pos[1][1]].setText("");
        placedPositions[pieceIdx] = null;
        renderPieces();
        setStatus("Peça #" + (pieceIdx+1) + " retirada", false);
    }

    /**
     * Actualitza visualment una casella del tauler amb un valor nou.
     * @param r     fila de la casella
     * @param c     columna de la casella
     * @param val   valor a mostrar
     * @param error si és true, mostra la casella en vermell
     */
    private void updateCell(int r, int c, int val, boolean error) {
        JLabel lbl = cellLabels[r][c];
        int ri = boardRegion[r][c];
        Color base = ri >= 0 ? REGION_COLORS[ri % REGION_COLORS.length] : new Color(245,245,245);
        lbl.setBackground(error ? COLOR_ERROR : base.darker());
        lbl.setText("<html><b style='font-size:14pt'>" + val + "</b></html>");
        lbl.setForeground(error ? Color.WHITE : new Color(30, 30, 30));
    }

    /**
     * Crida Prolog en un SwingWorker per calcular la solució del puzle actual
     * i l'aplica al tauler quan és disponible.
     */
    private void solveWithProlog() {
        String pid  = (String) puzzleCombo.getSelectedItem();
        String diff = (String) diffCombo.getSelectedItem();
        setStatus("Resolent...", false);

        SwingWorker<Map<String, Term>, Void> worker = new SwingWorker<>() {
            protected Map<String, Term> doInBackground() {
                String q = String.format(
                    "puzzle(%s, %s, R, P, _), solucio_pips(R, P, Sol)", pid, diff);
                Query query = new Query(q);
                return query.hasSolution() ? query.oneSolution() : null;
            }
            protected void done() {
                try {
                    Map<String, Term> sol = get();
                    if (sol == null) { setStatus("Sense solució", true); return; }
                    applySolution(sol.get("Sol"), sol.get("P"));
                    setStatus("Solució trobada!", false);
                } catch (Exception e) {
                    setStatus("Error Prolog: " + e.getMessage(), true);
                }
            }
        };
        worker.execute();
    }

    /**
     * Aplica la solució retornada per Prolog al tauler i actualitza la UI.
     * @param solTerm    terme Prolog amb la llista de posicions de les peces
     * @param piecesTerm terme Prolog amb la llista de peces
     */
    private void applySolution(Term solTerm, Term piecesTerm) {
        for (int r = 0; r < rows; r++) Arrays.fill(board[r], -1);
        Arrays.fill(placedPositions, null);
        parsePieces(piecesTerm);

        if (solTerm.isList()) {
            Term[] solutionArray = solTerm.toTermArray();
            for (int idx = 0; idx < solutionArray.length && idx < pieces.size(); idx++) {
                Term pos = solutionArray[idx];
                if (pos.isList()) {
                    Term[] coordPair = pos.toTermArray();
                    if (coordPair.length >= 2 && coordPair[0].isList() && coordPair[1].isList()) {
                        Term[] coord1 = coordPair[0].toTermArray();
                        Term[] coord2 = coordPair[1].toTermArray();
                        if (coord1.length >= 2 && coord2.length >= 2) {
                            int r1 = java.lang.Integer.parseInt(coord1[0].toString());
                            int c1 = java.lang.Integer.parseInt(coord1[1].toString());
                            int r2 = java.lang.Integer.parseInt(coord2[0].toString());
                            int c2 = java.lang.Integer.parseInt(coord2[1].toString());
                            int[] p = pieces.get(idx);
                            board[r1][c1] = p[0];
                            board[r2][c2] = p[1];
                            placedPositions[idx] = new int[][]{{r1,c1},{r2,c2}};
                        }
                    }
                }
            }
        }
        renderBoard();
        renderPieces();
    }

    /**
     * Comprova la solució manual actual cridant solucio_pips/3 a Prolog.
     * Mostra "✓ Correcte!" o "✗ Incorrecte!" i fa un efecte de flash al tauler.
     */
    private void checkSolution() {
        for (int i = 0; i < placedPositions.length; i++) {
            if (placedPositions[i] == null) {
                setStatus("Falta la peça #" + (i+1), true);
                return;
            }
        }

        String pid  = (String) puzzleCombo.getSelectedItem();
        String diff = (String) diffCombo.getSelectedItem();

        // Construeix el terme Prolog de la solució manual
        StringBuilder sb = new StringBuilder("[");
        for (int i = 0; i < placedPositions.length; i++) {
            int[][] pos = placedPositions[i];
            sb.append("[[").append(pos[0][0]).append(",").append(pos[0][1])
              .append("],[").append(pos[1][0]).append(",").append(pos[1][1]).append("]]");
            if (i < placedPositions.length - 1) sb.append(",");
        }
        sb.append("]");

        Query q = new Query(String.format(
            "puzzle(%s,%s,R,P,_), solucio_pips(R,P,%s)", pid, diff, sb.toString()));

        if (q.hasSolution()) {
            setStatus("✓ Correcte!", false);
            statusLabel.setForeground(new Color(100, 220, 100));
            flashBoard(COLOR_PLACED);
        } else {
            setStatus("✗ Incorrecte!", true);
            flashBoard(COLOR_ERROR);
        }
    }

    /**
     * Efecte visual de flash als contorns de les caselles ocupades.
     * @param color color del flash (verd per correcte, vermell per incorrecte)
     */
    private void flashBoard(Color color) {
        javax.swing.Timer t = new javax.swing.Timer(80, null);
        final int[] count = {0};
        t.addActionListener(e -> {
            boolean on = count[0] % 2 == 0;
            for (int r = 0; r < rows; r++)
                for (int c = 0; c < cols; c++)
                    if (boardRegion[r][c] >= 0 && board[r][c] >= 0)
                        cellLabels[r][c].setBorder(BorderFactory.createLineBorder(
                            on ? color : Color.GRAY, on ? 2 : 1));
            count[0]++;
            if (count[0] > 6) t.stop();
        });
        t.start();
    }

    /**
     * Reinicia el tauler: buida totes les caselles i retorna les peces a disponibles.
     */
    private void resetBoard() {
        for (int r = 0; r < rows; r++) Arrays.fill(board[r], -1);
        Arrays.fill(placedPositions, null);
        selectedPiece = -1;
        firstCell = null;
        renderBoard();
        renderPieces();
        setStatus("Reset", false);
    }

    /**
     * Comprova si dues caselles son adjacents (distància Manhattan = 1).
     * @param a coordenades [r,c] de la primera casella
     * @param b coordenades [r,c] de la segona casella
     * @return true si son adjacents
     */
    private boolean isAdjacent(int[] a, int[] b) {
        return (Math.abs(a[0]-b[0]) + Math.abs(a[1]-b[1])) == 1;
    }

    /**
     * Retorna l'etiqueta curta per a una condició de regió.
     * @param cond condició (sum, less, greater, equals, unequal, empty)
     * @param obj  objectiu numèric (-1 si no n'hi ha)
     * @return etiqueta (p.ex. "Σ=5", "≠", "=") o "" per a empty
     */
    private String condTag(String cond, int obj) {
        switch (cond) {
            case "sum":     return "Σ=" + obj;
            case "less":    return "Σ<" + obj;
            case "greater": return "Σ>" + obj;
            case "equals":  return "=";
            case "unequal": return "≠";
            default:        return "";
        }
    }

    /**
     * Actualitza el text d'estat a la barra superior.
     * @param msg   missatge a mostrar
     * @param error si és true, mostra el text en vermell
     */
    private void setStatus(String msg, boolean error) {
        statusLabel.setText(msg);
        statusLabel.setForeground(error ? new Color(255, 120, 120) : Color.LIGHT_GRAY);
    }

    /**
     * Crea un botó estilitzat amb hover.
     * @param text text del botó
     * @param bg   color de fons
     * @return JButton configurat
     */
    private JButton makeButton(String text, Color bg) {
        JButton b = new JButton(text);
        b.setBackground(bg);
        b.setForeground(Color.WHITE);
        b.setFont(new Font("SansSerif", Font.BOLD, 13));
        b.setBorder(BorderFactory.createEmptyBorder(8, 16, 8, 16));
        b.setFocusPainted(false);
        b.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        b.addMouseListener(new MouseAdapter() {
            public void mouseEntered(MouseEvent e) { b.setBackground(bg.brighter()); }
            public void mouseExited(MouseEvent e)  { b.setBackground(bg); }
        });
        return b;
    }

    /** Aplica estil fosc a un JComboBox. */
    private void styleCombo(JComboBox<String> c) {
        c.setBackground(new Color(50, 50, 65));
        c.setForeground(Color.WHITE);
        c.setFont(new Font("SansSerif", Font.PLAIN, 12));
    }

    /** Retorna text HTML amb color clar per a les etiquetes de la barra superior. */
    private String styledLabel(String text) {
        return "<html><font color='#cccccc'>" + text + "</font></html>";
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(PipsGUI::new);
    }
}