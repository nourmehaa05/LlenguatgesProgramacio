import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.JPL;
import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.List;

/**
 * PipsGUI.java
 * Interfície gràfica per al puzle Pips, integrada amb SWI-Prolog via JPL.
 *
 * Funcionalitats:
 *  - Seleccionar puzle per ID i dificultat
 *  - Mostrar el tauler buit amb les regions acolorides
 *  - Arrossegar peces de dominó al tauler
 *  - Resoldre automàticament amb Prolog
 *  - Comprovar si la solució manual és correcta
 *
 * Requisits:
 *  - SWI-Prolog instal·lat
 *  - JPL (jpl.jar) al classpath
 *  - pips.pl i puzzles.pl al directori de treball
 *
 * Compilar:
 *   javac -cp .:/usr/lib/swi-prolog/lib/jpl.jar PipsGUI.java
 * Executar:
 *   java -cp .:/usr/lib/swi-prolog/lib/jpl.jar \
 *        -Djava.library.path=/usr/lib/swi-prolog/lib/amd64 \
 *        PipsGUI
 */
public class PipsGUI extends JFrame {

    // ── Colors per a les regions ──────────────────────────────────────────
    private static final Color[] REGION_COLORS = {
        new Color(255, 220, 120),  // groc
        new Color(150, 210, 255),  // blau clar
        new Color(180, 255, 180),  // verd clar
        new Color(255, 180, 180),  // rosa
        new Color(200, 160, 255),  // lila
        new Color(255, 200, 140),  // taronja clar
        new Color(160, 240, 220),  // menta
        new Color(255, 160, 200),  // rosa fort
        new Color(200, 230, 255),  // blau molt clar
        new Color(230, 255, 160),  // llima
        new Color(255, 230, 200),  // melocotó
        new Color(200, 200, 200),  // gris
    };
    private static final Color COLOR_EMPTY    = new Color(245, 245, 245);
    private static final Color COLOR_SELECTED = new Color(100, 180, 255);
    private static final Color COLOR_PLACED   = new Color(80,  160, 80);
    private static final Color COLOR_ERROR    = new Color(255, 100, 100);
    private static final Color COLOR_BG       = new Color(40,  40,  50);

    // ── Estat del puzle ───────────────────────────────────────────────────
    private List<int[][]> regions;          // llista de caselles per regió
    private List<String>  regionConds;      // condicions: empty, sum, ...
    private List<java.lang.Integer> regionObjs;       // objectius numèrics
    private List<int[]>   pieces;           // peces [[v1,v2], ...]
    private int[][]       board;            // valors al tauler (-1 = buit)
    private int[][]       boardRegion;      // índex de regió per casella
    private int           rows, cols;
    private int           selectedPiece = -1;
    private int[]         firstCell     = null; // primera meitat seleccionada

    // Peces col·locades manualment: peça i -> posicions [[r1,c1],[r2,c2]]
    private int[][][] placedPositions;

    // ── Components UI ─────────────────────────────────────────────────────
    private JPanel     boardPanel;
    private JPanel     piecesPanel;
    private JLabel[][] cellLabels;
    private JLabel[]   pieceLabels;
    private JLabel     statusLabel;
    private JComboBox<String> puzzleCombo;
    private JComboBox<String> diffCombo;

    // ── IDs de puzles disponibles ─────────────────────────────────────────
    private static final String[] PUZZLE_IDS   = {
        "20250818","20250819","20250820","20250821",
        "20250822","20250823","20250824"
    };
    private static final String[] DIFFICULTIES = {"easy","medium","hard"};

    // ─────────────────────────────────────────────────────────────────────
    public PipsGUI() {
        super("Pips Puzzle");
        initProlog();
        buildUI();
        
        // Cargar el primer puzzle en un hilo separado después de que la UI esté lista
        SwingUtilities.invokeLater(() -> {
            try {
                Thread.sleep(500);
                loadPuzzle("20250818", "easy");
            } catch (InterruptedException e) {
                setStatus("Error carregant puzzle inicial", true);
            }
        });
    }

    // ── Inicialitza Prolog ────────────────────────────────────────────────
    private void initProlog() {
        try {
            String path = System.getProperty("user.dir").replace("\\", "/");
            String pipsFile = path + "/pips.pl";
            
            System.out.println("[DEBUG] Working directory: " + path);
            System.out.println("[DEBUG] Intentando cargar: " + pipsFile);
            
            Query q = new Query("consult('" + pipsFile + "')");
            if (q.hasSolution()) {
                System.out.println("[DEBUG] pips.pl cargado correctamente");
            } else {
                System.out.println("[DEBUG] No se pudo cargar pips.pl");
                throw new Exception("consult falló para " + pipsFile);
            }
        } catch (Exception e) {
            System.err.println("[ERROR] " + e.getMessage());
            e.printStackTrace();
            JOptionPane.showMessageDialog(null,
                "Error inicialitzant Prolog:\n" + e.getMessage() +
                "\n\nVerifica que pips.pl i puzzles.pl estan en:\n" +
                System.getProperty("user.dir"),
                "Error", JOptionPane.ERROR_MESSAGE);
        }
    }

    // ── Construcció de la interfície ──────────────────────────────────────
    private void buildUI() {
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setBackground(COLOR_BG);
        getContentPane().setBackground(COLOR_BG);
        setLayout(new BorderLayout(10, 10));

        // ── Barra superior ────────────────────────────────────────────────
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
        add(topPanel, BorderLayout.NORTH);

        // ── Tauler central ────────────────────────────────────────────────
        boardPanel = new JPanel();
        boardPanel.setBackground(COLOR_BG);
        JScrollPane boardScroll = new JScrollPane(boardPanel);
        boardScroll.setBackground(COLOR_BG);
        boardScroll.setBorder(BorderFactory.createEmptyBorder());
        add(boardScroll, BorderLayout.CENTER);

        // ── Panell de peces ───────────────────────────────────────────────
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

        // ── Barra d'estat ─────────────────────────────────────────────────
        statusLabel = new JLabel("  Selecciona una peça i col·loca-la al tauler.");
        statusLabel.setForeground(Color.LIGHT_GRAY);
        statusLabel.setFont(new Font("SansSerif", Font.PLAIN, 12));
        statusLabel.setBorder(BorderFactory.createEmptyBorder(4, 8, 4, 8));
        statusLabel.setOpaque(true);
        statusLabel.setBackground(new Color(20, 20, 30));
        add(statusLabel, BorderLayout.EAST);  // reaprofitat com a sidebar

        setSize(900, 650);
        setLocationRelativeTo(null);
        setVisible(true);
    }

    // ── Carrega un puzle des de Prolog ────────────────────────────────────
    private void loadPuzzle(String id, String diff) {
        try {
            String query = String.format(
                "puzzle(%s, %s, Regions, Peces, _)", id, diff);
            System.out.println("[DEBUG] Consultando Prolog: " + query);
            
            Query q = new Query(query);
            if (!q.hasSolution()) {
                System.out.println("[DEBUG] Prolog NO tiene solución para: " + query);
                setStatus("No s'ha trobat el puzle " + id + " (" + diff + "). "
                    + "Verifica que puzzles.pl està carregat.", true);
                return;
            }
            
            System.out.println("[DEBUG] Prolog encontró solución!");
            Map<String, Term> sol = q.oneSolution();
            Term regionsT = sol.get("Regions");
            Term pecesT = sol.get("Peces");
            
            System.out.println("[DEBUG] Regions: " + (regionsT != null ? "OK" : "NULL"));
            System.out.println("[DEBUG] Peces: " + (pecesT != null ? "OK" : "NULL"));
            
            if (regionsT == null || pecesT == null) {
                setStatus("Error: Regions o Peces són null.", true);
                return;
            }
            
            parseRegions(regionsT);
            parsePieces(pecesT);
            
            System.out.println("[DEBUG] Regiones parseadas: " + 
                (regions != null ? regions.size() : "NULL"));
            System.out.println("[DEBUG] Piezas parseadas: " + 
                (pieces != null ? pieces.size() : "NULL"));
            
            if (regions == null || regions.isEmpty()) {
                setStatus("Error: No s'han carregat les regions correctament.", true);
                return;
            }
            
            buildBoardFromRegions();
            System.out.println("[DEBUG] Board construido: " + rows + "x" + cols);
            
            renderBoard();
            renderPieces();
            selectedPiece = -1;
            firstCell = null;
            setStatus("Puzle " + id + " (" + diff + ") carregat. "
                + pieces.size() + " peces.", false);
        } catch (Exception e) {
            System.err.println("[ERROR] Exception en loadPuzzle: " + e.getMessage());
            e.printStackTrace();
            setStatus("Error carregant puzle: " + e.getMessage(), true);
        }
    }

    // ── Parseja les regions des del terme Prolog ──────────────────────────
    private void parseRegions(Term regionsTerm) {
        regions     = new ArrayList<>();
        regionConds = new ArrayList<>();
        regionObjs  = new ArrayList<>();

        System.out.println("[DEBUG] Parseando regionsTerm: " + regionsTerm);
        System.out.println("[DEBUG] isList: " + regionsTerm.isList());
        
        // JPL representa listas como términos con isLis() == true
        if (regionsTerm.isList()) {
            Term[] regionArray = regionsTerm.toTermArray();
            System.out.println("[DEBUG] Array de regiones size: " + regionArray.length);
            
            for (int i = 0; i < regionArray.length; i++) {
                Term reg = regionArray[i];
                System.out.println("[DEBUG]   Region #" + (i+1) + ": " + reg);
                
                String cond = reg.arg(1).toString();
                Term   objT = reg.arg(2);
                int    obj  = objT.toString().equals("nil") ? -1
                            : java.lang.Integer.parseInt(objT.toString());
                Term cellsT = reg.arg(3);

                List<int[]> cells = new ArrayList<>();
                if (cellsT.isList()) {
                    Term[] cellArray = cellsT.toTermArray();
                    for (Term cellTerm : cellArray) {
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
        System.out.println("[DEBUG] Total regiones parseadas: " + regions.size());
    }

    // ── Parseja les peces des del terme Prolog ────────────────────────────
    private void parsePieces(Term piecesTerm) {
        pieces = new ArrayList<>();
        
        System.out.println("[DEBUG] Parseando piecesTerm: " + piecesTerm);
        System.out.println("[DEBUG] isList: " + piecesTerm.isList());
        
        if (piecesTerm.isList()) {
            Term[] pieceArray = piecesTerm.toTermArray();
            System.out.println("[DEBUG] Array de piezas size: " + pieceArray.length);
            
            for (int i = 0; i < pieceArray.length; i++) {
                Term piece = pieceArray[i];
                System.out.println("[DEBUG]   Pieza #" + (i+1) + ": " + piece);
                
                Term[] vals = piece.toTermArray();
                if (vals.length >= 2) {
                    int v1 = java.lang.Integer.parseInt(vals[0].toString());
                    int v2 = java.lang.Integer.parseInt(vals[1].toString());
                    pieces.add(new int[]{v1, v2});
                }
            }
        }
        System.out.println("[DEBUG] Total piezas parseadas: " + pieces.size());
    }

    // ── Construeix el tauler a partir de les regions ──────────────────────
    private void buildBoardFromRegions() {
        rows = 0; cols = 0;
        for (int[][] reg : regions) {
            for (int[] cell : reg) {
                rows = Math.max(rows, cell[0] + 1);
                cols = Math.max(cols, cell[1] + 1);
            }
        }
        board        = new int[rows][cols];
        boardRegion  = new int[rows][cols];
        placedPositions = new int[pieces.size()][][];

        for (int[] row : board)       Arrays.fill(row, -1);
        for (int[] row : boardRegion) Arrays.fill(row, -1);

        for (int i = 0; i < regions.size(); i++) {
            for (int[] cell : regions.get(i)) {
                boardRegion[cell[0]][cell[1]] = i;
            }
        }
    }

    // ── Renderitza el tauler ──────────────────────────────────────────────
    private void renderBoard() {
        boardPanel.removeAll();
        
        // Validació: si no hay regiones cargadas, no hacer nada
        if (rows == 0 || cols == 0) {
            JLabel errorLabel = new JLabel("No s'han carregat les regions del puzle.");
            errorLabel.setForeground(new Color(255, 100, 100));
            errorLabel.setFont(new Font("SansSerif", Font.PLAIN, 14));
            boardPanel.add(errorLabel);
            boardPanel.revalidate();
            boardPanel.repaint();
            return;
        }
        
        // Aumentar tamaño de celda para mejor visualización
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
                lbl.setBorder(BorderFactory.createLineBorder(
                    new Color(40, 40, 50), 2));

                int ri = boardRegion[r][c];
                if (ri >= 0) {
                    lbl.setBackground(REGION_COLORS[ri % REGION_COLORS.length]);
                    lbl.setForeground(new Color(30, 30, 30));
                    // Etiqueta de condició a la primera casella
                    if (regions.get(ri)[0][0] == r
                            && regions.get(ri)[0][1] == c) {
                        String cond = regionConds.get(ri);
                        int    obj  = regionObjs.get(ri);
                        String tag  = condTag(cond, obj);
                        if (!tag.isEmpty()) {
                            lbl.setToolTipText(tag);
                            lbl.setText("<html><div style='font-size:10pt;"
                                + "color:#222;font-weight:bold'>" + tag + "</div></html>");
                        }
                    }
                } else {
                    lbl.setBackground(new Color(20, 20, 30));
                }

                if (board[r][c] >= 0) {
                    int val = board[r][c];
                    lbl.setText("<html><b style='font-size:18pt;color:white'>"
                        + val + "</b></html>");
                    lbl.setBackground(lbl.getBackground().darker());
                }

                final int fr = r, fc = c;
                lbl.addMouseListener(new MouseAdapter() {
                    public void mouseClicked(MouseEvent e) {
                        onCellClick(fr, fc);
                    }
                    public void mouseEntered(MouseEvent e) {
                        if (boardRegion[fr][fc] >= 0
                                && selectedPiece >= 0
                                && board[fr][fc] < 0) {
                            lbl.setBorder(BorderFactory.createLineBorder(
                                COLOR_SELECTED, 3));
                        }
                    }
                    public void mouseExited(MouseEvent e) {
                        lbl.setBorder(BorderFactory.createLineBorder(
                            new Color(40, 40, 50), 2));
                    }
                });
                cellLabels[r][c] = lbl;
                grid.add(lbl);
            }
        }

        // Llegenda de regions
        JPanel legend = buildLegend();
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0; gbc.gridy = 0; gbc.insets = new Insets(15,15,15,15);
        boardPanel.add(grid, gbc);
        gbc.gridx = 1; gbc.anchor = GridBagConstraints.NORTH;
        boardPanel.add(legend, gbc);

        boardPanel.revalidate();
        boardPanel.repaint();
    }

    // ── Llegenda de regions ───────────────────────────────────────────────
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
            if (i < regions.size() - 1) {
                p.add(Box.createVerticalStrut(2));
            }
        }
        p.setMaximumSize(new Dimension(150, p.getPreferredSize().height));
        return p;
    }

    // ── Renderitza les peces ──────────────────────────────────────────────
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

            // Marcar si ja esta col·locada
            if (placedPositions[i] != null) {
                piecePanel.setBackground(new Color(40, 80, 40));
                piecePanel.setBorder(BorderFactory.createCompoundBorder(
                    BorderFactory.createLineBorder(COLOR_PLACED, 3),
                    BorderFactory.createEmptyBorder(6, 6, 6, 6)));
            }

            piecePanel.addMouseListener(new MouseAdapter() {
                public void mouseClicked(MouseEvent e) {
                    if (placedPositions[idx] != null) {
                        removePiece(idx);
                    } else {
                        selectPiece(idx);
                    }
                }
                public void mouseEntered(MouseEvent e) {
                    if (placedPositions[idx] == null) {
                        piecePanel.setBorder(BorderFactory.createCompoundBorder(
                            BorderFactory.createLineBorder(COLOR_SELECTED, 3),
                            BorderFactory.createEmptyBorder(6, 6, 6, 6)));
                    }
                }
                public void mouseExited(MouseEvent e) {
                    Color border = placedPositions[idx] != null
                        ? COLOR_PLACED : Color.GRAY;
                    piecePanel.setBorder(BorderFactory.createCompoundBorder(
                        BorderFactory.createLineBorder(border,
                            placedPositions[idx] != null ? 3 : 2),
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

    // ── Crea una etiqueta amb el valor d'un pip ───────────────────────────
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

    // ── Selecciona una peça ───────────────────────────────────────────────
    private void selectPiece(int idx) {
        selectedPiece = idx;
        firstCell = null;
        int[] p = pieces.get(idx);
        setStatus("Peça #" + (idx+1) + " seleccionada [" + p[0] + "|" + p[1]
            + "]. Clica la primera casella.", false);
        renderPieces();
        highlightSelected();
    }

    private void highlightSelected() {
        if (pieceLabels == null) return;
        // ressaltar la peça seleccionada visualment
        renderPieces();
    }

    // ── Click a una casella del tauler ────────────────────────────────────
    private void onCellClick(int r, int c) {
        if (selectedPiece < 0) {
            setStatus("Primer selecciona una peça.", false);
            return;
        }
        if (boardRegion[r][c] < 0) {
            setStatus("Casella fora del tauler.", true);
            return;
        }
        if (board[r][c] >= 0) {
            setStatus("Casella ja ocupada.", true);
            return;
        }

        if (firstCell == null) {
            // Primera meitat
            firstCell = new int[]{r, c};
            cellLabels[r][c].setBackground(COLOR_SELECTED);
            cellLabels[r][c].setText("<html><b style='color:white'>?</b></html>");
            setStatus("Primera meitat a [" + r + "," + c
                + "]. Ara clica una casella adjacent.", false);
        } else {
            // Segona meitat: comprova adjacència
            if (!isAdjacent(firstCell, new int[]{r, c})) {
                setStatus("Les dues caselles no son adjacents!", true);
                // Desfer primera selecció
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

    // ── Col·loca una peça al tauler ───────────────────────────────────────
    private void placePiece(int pieceIdx, int[] cell1, int[] cell2) {
        int[] p = pieces.get(pieceIdx);
        board[cell1[0]][cell1[1]] = p[0];
        board[cell2[0]][cell2[1]] = p[1];
        placedPositions[pieceIdx] = new int[][]{cell1, cell2};

        updateCell(cell1[0], cell1[1], p[0], false);
        updateCell(cell2[0], cell2[1], p[1], false);

        renderPieces();
        setStatus("Peça #" + (pieceIdx+1) + " [" + p[0] + "|" + p[1]
            + "] col·locada.", false);

        // Comprova si totes les peces estan col·locades
        boolean all = true;
        for (int[][] pos : placedPositions) if (pos == null) { all = false; break; }
        if (all) setStatus("Totes les peces col·locades! Prem Comprovar.", false);
    }

    // ── Elimina una peça del tauler ───────────────────────────────────────
    private void removePiece(int pieceIdx) {
        if (placedPositions[pieceIdx] == null) return;
        int[][] pos = placedPositions[pieceIdx];
        board[pos[0][0]][pos[0][1]] = -1;
        board[pos[1][0]][pos[1][1]] = -1;
        int ri0 = boardRegion[pos[0][0]][pos[0][1]];
        int ri1 = boardRegion[pos[1][0]][pos[1][1]];
        cellLabels[pos[0][0]][pos[0][1]]
            .setBackground(REGION_COLORS[ri0 % REGION_COLORS.length]);
        cellLabels[pos[0][0]][pos[0][1]].setText("");
        cellLabels[pos[1][0]][pos[1][1]]
            .setBackground(REGION_COLORS[ri1 % REGION_COLORS.length]);
        cellLabels[pos[1][0]][pos[1][1]].setText("");
        placedPositions[pieceIdx] = null;
        renderPieces();
        setStatus("Peça #" + (pieceIdx+1) + " retirada.", false);
    }

    // ── Actualitza una casella visual ─────────────────────────────────────
    private void updateCell(int r, int c, int val, boolean error) {
        JLabel lbl = cellLabels[r][c];
        int ri = boardRegion[r][c];
        Color base = ri >= 0
            ? REGION_COLORS[ri % REGION_COLORS.length]
            : COLOR_EMPTY;
        lbl.setBackground(error ? COLOR_ERROR : base.darker());
        lbl.setText("<html><b style='font-size:14pt'>" + val + "</b></html>");
        lbl.setForeground(error ? Color.WHITE : new Color(30, 30, 30));
    }

    // ── Resol el puzle amb Prolog ─────────────────────────────────────────
    private void solveWithProlog() {
        String pid  = (String) puzzleCombo.getSelectedItem();
        String diff = (String) diffCombo.getSelectedItem();
        setStatus("Resolent amb Prolog...", false);

        SwingWorker<Map<String, Term>, Void> worker = new SwingWorker<>() {
            protected Map<String, Term> doInBackground() {
                String q = String.format(
                    "puzzle(%s, %s, R, P, _), solucio_pips(R, P, Sol)",
                    pid, diff);
                Query query = new Query(q);
                return query.hasSolution() ? query.oneSolution() : null;
            }
            protected void done() {
                try {
                    Map<String, Term> sol = get();
                    if (sol == null) {
                        setStatus("Prolog no ha trobat solució.", true);
                        return;
                    }
                    applySolution(sol.get("Sol"), sol.get("P"));
                    setStatus("Solució trobada per Prolog!", false);
                } catch (Exception e) {
                    setStatus("Error Prolog: " + e.getMessage(), true);
                }
            }
        };
        worker.execute();
    }

    // ── Aplica la solució de Prolog al tauler ─────────────────────────────
    private void applySolution(Term solTerm, Term piecesTerm) {
        // Netejar tauler
        for (int r = 0; r < rows; r++)
            Arrays.fill(board[r], -1);
        Arrays.fill(placedPositions, null);

        // Reparseja les peces (per si no estan instanciades igual)
        parsePieces(piecesTerm);

        System.out.println("[DEBUG] Aplicando solución de Prolog");
        System.out.println("[DEBUG] solTerm: " + solTerm);
        System.out.println("[DEBUG] isList: " + solTerm.isList());
        
        if (solTerm.isList()) {
            Term[] solutionArray = solTerm.toTermArray();
            System.out.println("[DEBUG] Tamaño de solución: " + solutionArray.length);
            
            for (int idx = 0; idx < solutionArray.length && idx < pieces.size(); idx++) {
                Term pos = solutionArray[idx];  // [[r1,c1],[r2,c2]]
                System.out.println("[DEBUG]   Posición #" + (idx+1) + ": " + pos);
                
                if (pos.isList()) {
                    Term[] coordPair = pos.toTermArray();
                    if (coordPair.length >= 2) {
                        Term cell1 = coordPair[0];
                        Term cell2 = coordPair[1];
                        
                        if (cell1.isList() && cell2.isList()) {
                            Term[] coord1 = cell1.toTermArray();
                            Term[] coord2 = cell2.toTermArray();
                            
                            if (coord1.length >= 2 && coord2.length >= 2) {
                                int r1 = java.lang.Integer.parseInt(coord1[0].toString());
                                int c1 = java.lang.Integer.parseInt(coord1[1].toString());
                                int r2 = java.lang.Integer.parseInt(coord2[0].toString());
                                int c2 = java.lang.Integer.parseInt(coord2[1].toString());

                                int[] p = pieces.get(idx);
                                board[r1][c1] = p[0];
                                board[r2][c2] = p[1];
                                placedPositions[idx] = new int[][]{{r1,c1},{r2,c2}};
                                System.out.println("[DEBUG]     Pieza #" + (idx+1) + " [" + p[0] + "|" + p[1] + 
                                    "] -> [" + r1 + "," + c1 + "] [" + r2 + "," + c2 + "]");
                            }
                        }
                    }
                }
            }
        }
        
        renderBoard();
        renderPieces();
        System.out.println("[DEBUG] Solución aplicada y renderizada");
    }

    // ── Comprova la solució manual ────────────────────────────────────────
    private void checkSolution() {
        // Construeix el terme Solucio des de placedPositions
        for (int i = 0; i < placedPositions.length; i++) {
            if (placedPositions[i] == null) {
                setStatus("Falten peces per col·locar (#" + (i+1) + ").", true);
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
              .append("],[").append(pos[1][0]).append(",").append(pos[1][1])
              .append("]]");
            if (i < placedPositions.length - 1) sb.append(",");
        }
        sb.append("]");

        String qStr = String.format(
            "puzzle(%s,%s,R,P,_), solucio_pips(R,P,%s)",
            pid, diff, sb.toString());

        Query q = new Query(qStr);
        if (q.hasSolution()) {
            setStatus("✓ Solució CORRECTA!", false);
            statusLabel.setForeground(new Color(100, 220, 100));
            flashBoard(COLOR_PLACED);
        } else {
            setStatus("✗ Solució INCORRECTA. Revisa les restriccions.", true);
            flashBoard(COLOR_ERROR);
        }
    }

    // ── Efecte visual de flash al tauler ──────────────────────────────────
    private void flashBoard(Color color) {
        javax.swing.Timer t = new javax.swing.Timer(80, null);
        final int[] count = {0};
        t.addActionListener(e -> {
            boolean on = count[0] % 2 == 0;
            for (int r = 0; r < rows; r++)
                for (int c = 0; c < cols; c++)
                    if (boardRegion[r][c] >= 0 && board[r][c] >= 0)
                        cellLabels[r][c].setBorder(
                            BorderFactory.createLineBorder(
                                on ? color : Color.GRAY, on ? 2 : 1));
            count[0]++;
            if (count[0] > 6) t.stop();
        });
        t.start();
    }

    // ── Reset del tauler ──────────────────────────────────────────────────
    private void resetBoard() {
        for (int r = 0; r < rows; r++) Arrays.fill(board[r], -1);
        Arrays.fill(placedPositions, null);
        selectedPiece = -1;
        firstCell = null;
        renderBoard();
        renderPieces();
        setStatus("Tauler reiniciat.", false);
    }

    // ── Utilitats ─────────────────────────────────────────────────────────
    private boolean isAdjacent(int[] a, int[] b) {
        int dr = Math.abs(a[0] - b[0]);
        int dc = Math.abs(a[1] - b[1]);
        return (dr + dc) == 1;
    }

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

    private void setStatus(String msg, boolean error) {
        statusLabel.setText("  " + msg);
        statusLabel.setForeground(error
            ? new Color(255, 120, 120) : Color.LIGHT_GRAY);
    }

    private JButton makeButton(String text, Color bg) {
        JButton b = new JButton(text);
        b.setBackground(bg);
        b.setForeground(Color.WHITE);
        b.setFont(new Font("SansSerif", Font.BOLD, 13));
        b.setBorder(BorderFactory.createEmptyBorder(8, 16, 8, 16));
        b.setFocusPainted(false);
        b.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        b.addMouseListener(new MouseAdapter() {
            public void mouseEntered(MouseEvent e) {
                b.setBackground(bg.brighter());
            }
            public void mouseExited(MouseEvent e) {
                b.setBackground(bg);
            }
        });
        return b;
    }

    private void styleCombo(JComboBox<String> c) {
        c.setBackground(new Color(50, 50, 65));
        c.setForeground(Color.WHITE);
        c.setFont(new Font("SansSerif", Font.PLAIN, 12));
    }

    private String styledLabel(String text) {
        return "<html><font color='#cccccc'>" + text + "</font></html>";
    }

    // ── Main ──────────────────────────────────────────────────────────────
    public static void main(String[] args) {
        SwingUtilities.invokeLater(PipsGUI::new);
    }
}