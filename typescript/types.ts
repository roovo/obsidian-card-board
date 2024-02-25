export type CardBoardPluginSettings = CardBoardPluginSettingsPreV11 | CardBoardPluginSettingsPostV11;

type AutoCompleteTag =
  { tag : "FalseSpecified" | "NotSpecifed" | "TrueSpecified" }

type AutoComplete =
  { tag : "AutoCompleteTag", data : AutoCompleteTag }

type CompletionTag =
  { tag : "Completed" } |
  { tag : "CompletedAt", data : number } |
  { tag : "Incomplete" }

type Completion =
  { tag : "CompletedTag", data : number }

type DueTag =
  { tag : "NotSet" } |
  { tag : "SetToDate", date : number } |
  { tag : "SetToNone" }

type Due = {
  tag : "DueTag",
  data : DueTag
}

type Tag = {
  tag : "ObsidianTag",
  data : string
}

type Word =
  { tag : "Word", data : string }

type Content = AutoComplete | Completion | Due | Tag | Word;

export type TaskItemFields = {
  autoComplete : AutoCompleteTag;
  completion : CompletionTag;
  contents : Content[];
  dueFile : number | null;
  dueTag : DueTag;
  filePath : string;
  lineNumber : number;
  notes : string;
  originalBlock : string;
  originalLine : string;
  tags : string[];
  title : string[];
}

export type TaskItem = {
  fields : TaskItemFields;
  subFields : TaskItemFields[];
}

export type CardBoardPluginSettingsPostV11 = {
  data : {
    boardConfigs : ({
      columns : ({
        tag : "completed"
        data : {
          collapsed : boolean;
          name : string;
          index : number;
          limit : number;
        }
      } | {
        tag : "dated"
        data : {
          collapsed : boolean;
          name : string;
          range : ({
            tag : "after"
            data : number
          } | {
            tag : "before"
            data : number
          } | {
            tag : "between"
            data : {
              from : number
              to : number
            }
          })
        }
      } | {
        tag : "namedTag"
        data : {
          collapsed : boolean;
          name : string;
          tag : string;
        }
      } | {
        tag : "otherTags"
        data : {
          collapsed : boolean;
          name : string;
        }
      } | {
        tag : "undated"
        data : {
          collapsed : boolean;
          name : string;
        }
      } | {
        tag : "untagged"
        data : {
          collapsed : boolean;
          name : string;
        }
      })[];
      filters : Filter[];
      filterPolarity : "Allow" | "Deny";
      showColumnTags : boolean;
      showFilteredTags : boolean;
      name : string
    })[];
    globalSettings : {
      defaultColumnNames : {
        today : string;
        tomorrow : string;
        future : string;
        undated : string;
        otherTags : string;
        untagged : string
        completed : string;
      }
      filters : Filter[];
      firstDayOfWeek : "FromLocale" | "Mon" | "Tue" | "Wed" | "Thu" | "Fri" | "Sat" | "Sun";
      ignoreFileNameDates : boolean;
      taskCompletionFormat : "NoCompletion" | "ObsidianCardBoard" | "ObsidianDataview" | "ObsidianTasks";
      taskCompletionInLocalTime : boolean;
      taskCompletionShowUtcOffset : boolean;
    }
  };
  version : string
}

type CardBoardPluginSettingsPreV11 = {
  data : {
    boardConfigs : (
      { data : {
          columns : { displayTitle : string; tag : string }[];
          showColumnTags : boolean;
          completedCount : number;
          filters : ({ data : string; tag : "tagFilter" } | { data : string; tag : "pathFilter" } | { data : string; tag : "fileFilter" })[];
          filterPolarity : "Allow" | "Deny";
          showFilteredTags : boolean;
          includeOthers : boolean;
          includeUntagged : boolean;
          title : string
        };
        tag : "tagBoardConfig" }
      | { data : {
          completedCount : number;
          filters : Filter[];
          filterPolarity : "Allow" | "Deny";
          showFilteredTags : boolean;
          includeUndated : boolean;
          title : string
        };
        tag : "dateBoardConfig"
      }
    )[];
    globalSettings : {
      taskCompletionFormat : "NoCompletion" | "ObsidianCardBoard" | "ObsidianDataview" | "ObsidianTasks";
      columnNames : {
        today : string;
        tomorrow : string;
        future : string;
        undated : string;
        others : string;
        untagged : string
        completed : string;
      }
      ignoreFileNameDates : boolean;
    }
  };
  version : string
}

export type Filter = { data : string; tag : "tagFilter" } | { data : string; tag : "pathFilter" } | { data : string; tag : "fileFilter" };
