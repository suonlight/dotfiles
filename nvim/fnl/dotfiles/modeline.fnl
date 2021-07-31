(module dotfiles.modeline
  {autoload {nvim aniseed.nvim
             util dotfiles.util
             galaxyline galaxyline
             file-info galaxyline.provider_fileinfo}
   require-macros [dotfiles.macros]})

(set galaxyline.short_line_list [ "NvimTree" "vista" "dbui" "packer"]) ;; missing this line causes slow perf on ruby

(local section galaxyline.section)

(local onedark {
        :black "#0c0e15"
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
    :diff_add  "#27341c"
  :diff_delete "#331c1e"
  :diff_change "#102b40"
    :diff_text "#1c4a6e"
})

(local colors {
  :bg onedark.bg0
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
  :black onedark.black
})

(defn buffer-not-empty []
  (not= (nvim.fn.empty (util.expand "%:t")) 1))

(defn mode-color []
  (let [mode-colors {:n colors.green
                     :i colors.blue
                     :c colors.orange
                     :V colors.magenta
                     :v colors.magenta
                     "" colors.magenta
                     :R colors.red}]
    (or (?. mode-colors (nvim.fn.mode)) colors.red)))

(defn vi-mode []
  (let [alias {:n "NORMAL"
               :i "INSERT"
               :c "COMMAND"
               :V "VISUAL"
               :v "VISUAL"
               "" "VISUAL"
               :R "REPLACE"}
        alias-mode (or (?. alias (nvim.fn.mode)) (nvim.fn.mode))]
    (nvim.command (.. "hi GalaxyViMode guibg=" (mode-color)))
    (.. "  " alias-mode " ")))

(set section.left [{:ViMode {:provider (fn [] (vi-mode))
                             :separator " "
                             :highlight [colors.bg colors.section_bg]
                             :separator_highlight [colors.bg colors.section_bg]}}
                   {:FileIcon {:provider "FileIcon"
                               :condition buffer-not-empty
                               :highlight [(file-info.get_file_icon_color) colors.section_bg]}}
                   {:FileName {:provider (fn [] (util.expand "%f"))
                               :condition buffer-not-empty
                               :highlight [colors.fg colors.section_bg ]
                               :separator_highlight [colors.fg colors.section_bg]}}])

(defn line-column []
  (let [line (nvim.fn.line ".")
        column (nvim.fn.col ".")
        max-lines (nvim.fn.line "$")]
    (nvim.command (.. "hi GalaxyLineColumn guibg=" (mode-color)))
    (.. " " line "/" max-lines ":" column)))

(set section.right [{:LineColumn {:provider (fn [] (line-column))
                                  :separator " "
                                  :highlight [colors.black mode-color]
                                  :separator_highlight [colors.bg colors.section_bg]}}])

(set section.short_line_left [{:SpacerInactive {:provider (fn [] "  ")
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

(set section.short_line_right [{:LineColumnInactive {:provider "LineColumn"
                                                     :separator " "
                                                     :highlight [colors.fg colors.bg_inactive]
                                                     :separator_highlight [colors.fg colors.bg_inactive]}}])

(defn setup [] (galaxyline.load_galaxyline))
