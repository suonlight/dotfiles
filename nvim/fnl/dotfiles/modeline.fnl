;; Localize required modules and APIs
(local galaxyline (require :galaxyline))
(local section galaxyline.section)
(local file-info (require :galaxyline.provider_fileinfo))
(local condition (require :galaxyline.condition))
(local util (require :dotfiles.util))

;; Set global galaxyline properties
(set galaxyline.short_line_list ["NvimTree" "vista" "dbui" "packer"])

;; Define color palette
(local onedark {:black "#0c0e15"
                :bg0 "#1a212e"
                :bg1 "#21283b"
                :bg2 "#283347"
                :bg3 "#2a324a"
                :bg_d "#141b24"
                :bg_blue "#54b0fd"
                :bg_yellow "#f2cc81"
                :fg "#93a4c3"
                :purple "#c75ae8"
                :green "#8bcd5b"
                :orange "#dd9046"
                :blue "#41a7fc"
                :yellow "#efbd5d"
                :cyan "#34bfd0"
                :red "#f65866"
                :grey "#455574"
                :dark_cyan "#1b6a73"
                :dark_red "#992525"
                :dark_yellow "#8f610d"
                :dark_purple "#862aa1"
                :diff_add "#27341c"
                :diff_delete "#331c1e"
                :diff_change "#102b40"
                :diff_text "#1c4a6e"})

(local colors {:bg onedark.bg0
               :bg_inactive onedark.bg3
               :fg onedark.fg
               :fg_focus "#f8f8f2"
               :section_bg onedark.bg0
               :yellow onedark.bg_yellow
               :cyan onedark.cyan
               :green onedark.green
               :orange onedark.orange
               :magenta onedark.purple
               :blue onedark.blue
               :red onedark.red
               :black onedark.black})

;; --- Provider Functions ---

(fn buffer-not-empty []
  (not= (vim.fn.empty (util.expand "%:t")) 1))

(fn mode-color []
  (let [current-mode (vim.fn.mode)
        mode-colors {:n colors.green
                     :i colors.blue
                     :c colors.orange
                     :V colors.magenta
                     :v colors.magenta
                     "" colors.magenta ; Visual Block
                     :R colors.red}]
    (or (. mode-colors current-mode) colors.red)))

(fn vi-mode []
  (let [current-mode (vim.fn.mode)
        alias {:n "NORMAL"
               :i "INSERT"
               :c "COMMAND"
               :V "VISUAL"
               :v "VISUAL"
               "" "VISUAL"
               :R "REPLACE"}
        alias-mode (or (. alias current-mode) current-mode)]
    (vim.cmd (.. "hi GalaxyViMode guibg=" (mode-color)))
    (.. "  " alias-mode " ")))

(fn line-column []
  (let [line (vim.fn.line ".")
        column (vim.fn.col ".")
        max-lines (vim.fn.line "$")]
    (vim.cmd (.. "hi GalaxyLineColumn guibg=" (mode-color)))
    (.. " " line "/" max-lines ":" column)))

;; --- Section Definitions ---

(set section.left
     [{:ViMode {:provider vi-mode
                :separator " "
                :highlight [colors.bg colors.section_bg]
                :separator_highlight [colors.bg colors.section_bg]}}
      {:FileSize {:provider "FileSize"
                  :condition buffer-not-empty
                  :highlight [colors.fg colors.section_bg]}}
      {:FileIcon {:provider "FileIcon"
                  :condition buffer-not-empty
                  :highlight [(file-info.get_file_icon_color) colors.section_bg]}}
      {:FileName {:provider (fn [] (util.expand "%f"))
                  :condition buffer-not-empty
                  :highlight [colors.fg colors.section_bg]
                  :separator_highlight [colors.fg colors.section_bg]}}])

(set section.right
     [{:LspStatus {:provider (fn []
                               (if (not= (next (vim.lsp.get_clients)) nil)
                                 "    LSP"
                                 ""))}}
      {:FileFormat {:provider "FileFormat"
                    :condition condition.hide_in_width
                    :separator " "
                    :highlight [colors.cyan colors.bg_inactive]
                    :separator_highlight [colors.fg colors.section_bg]}}
      {:BufferType {:provider "FileTypeName"
                    :condition condition.hide_in_width
                    :separator " "
                    :highlight [colors.cyan colors.bg_inactive]
                    :separator_highlight [colors.fg colors.section_bg]}}
      {:GitIcon {:provider (fn [] "  ")
                 :condition condition.check_git_workspace
                 :separator " "
                 :highlight [colors.fg colors.section_bg]}}
      {:GitBranch {:provider "GitBranch"
                   :condition condition.check_git_workspace
                   :separator ""
                   :highlight [colors.fg colors.section_bg]}}
      {:LineColumn {:provider line-column
                    :separator " "
                    :highlight [colors.black (mode-color)]
                    :separator_highlight [colors.bg colors.section_bg]}}])

(set section.short_line_left
     [{:SpacerInactive {:provider (fn [] "  ")
                        :highlight [colors.fg colors.bg_inactive]
                        :separator_highlight [colors.fg colors.bg_inactive]}}
      {:FileIconInactive {:provider "FileIcon"
                          :condition buffer-not-empty
                          :separator " "
                          :highlight [colors.fg colors.bg_inactive]
                          :separator_highlight [(file-info.get_file_icon_color) colors.bg_inactive]}}
      {:FileNameInactive {:provider (fn [] (util.expand "%f"))
                          :separator " "
                          :highlight [colors.fg colors.bg_inactive]
                          :separator_highlight [colors.fg colors.bg_inactive]}}])

(set section.short_line_right
     [{:FileFormatInactive {:provider "FileFormat"
                            :condition condition.hide_in_width
                            :separator " "
                            :highlight [colors.fg colors.bg_inactive]
                            :separator_highlight [colors.fg colors.bg_inactive]}}
      {:BufferTypeInactive {:provider "FileTypeName"
                            :condition condition.hide_in_width
                            :separator " "
                            :highlight [colors.fg colors.bg_inactive]
                            :separator_highlight [colors.fg colors.bg_inactive]}}
      {:LineColumnInactive {:provider "LineColumn"
                            :separator " "
                            :highlight [colors.fg colors.bg_inactive]
                            :separator_highlight [colors.fg colors.bg_inactive]}}])


;; --- EXPORT PUBLIC SETUP FUNCTION ---
{:setup (fn []
          (galaxyline.load_galaxyline))}
