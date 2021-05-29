/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.tools.internal.generator.types.autogen;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

import java.util.List;
import java.util.Objects;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "fieldId",
    "category",
    "labels",
    "type",
    "dependentField",
    "deprecated",
    "dependFor",
    "displayName",
    "sortable",
    "dropdownSupported",
    "isPremium"
})
public class ScreenerFieldDefinition implements Comparable<ScreenerFieldDefinition>
{
    @JsonProperty("fieldId")
    private String fieldId;
    @JsonProperty("category")
    private Category category;
    @JsonProperty("labels")
    private List<Label> labels = null;
    @JsonProperty("type")
    private String type;
    @JsonProperty("dependentField")
    private String dependentField;
    @JsonProperty("deprecated")
    private Boolean deprecated;
    @JsonProperty("dependFor")
    private List<String> dependFor = null;
    @JsonProperty("displayName")
    private String displayName;
    @JsonProperty("sortable")
    private Boolean sortable;
    @JsonProperty("dropdownSupported")
    private Boolean dropdownSupported;
    @JsonProperty("isPremium")
    private Boolean isPremium;

    @JsonProperty("fieldId")
    public String getFieldId() {
        return fieldId;
    }

    @JsonProperty("fieldId")
    public void setFieldId(String fieldId) {
        this.fieldId = fieldId;
    }

    @JsonProperty("category")
    public Category getCategory() {
        return category;
    }

    @JsonProperty("category")
    public void setCategory(Category category) {
        this.category = category;
    }

    @JsonProperty("labels")
    public List<Label> getLabels() {
        return labels;
    }

    @JsonProperty("labels")
    public void setLabels(List<Label> labels) {
        this.labels = labels;
    }

    @JsonProperty("type")
    public String getType() {
        return type;
    }

    @JsonProperty("type")
    public void setType(String type) {
        this.type = type;
    }

    @JsonProperty("dependentField")
    public String getDependentField() {
        return dependentField;
    }

    @JsonProperty("dependentField")
    public void setDependentField(String dependentField) {
        this.dependentField = dependentField;
    }

    @JsonProperty("deprecated")
    public Boolean getDeprecated() {
        return deprecated;
    }

    @JsonProperty("deprecated")
    public void setDeprecated(Boolean deprecated) {
        this.deprecated = deprecated;
    }

    @JsonProperty("dependFor")
    public List<String> getDependFor() {
        return dependFor;
    }

    @JsonProperty("dependFor")
    public void setDependFor(List<String> dependFor) {
        this.dependFor = dependFor;
    }

    @JsonProperty("displayName")
    public String getDisplayName() {
        return displayName;
    }

    @JsonProperty("displayName")
    public void setDisplayName(String displayName) {
        this.displayName = displayName;
    }

    @JsonProperty("sortable")
    public Boolean getSortable() {
        return sortable;
    }

    @JsonProperty("sortable")
    public void setSortable(Boolean sortable) {
        this.sortable = sortable;
    }

    @JsonProperty("dropdownSupported")
    public Boolean getDropdownSupported() {
        return dropdownSupported;
    }

    @JsonProperty("dropdownSupported")
    public void setDropdownSupported(Boolean dropdownSupported) {
        this.dropdownSupported = dropdownSupported;
    }

    @JsonProperty("isPremium")
    public Boolean getIsPremium() {
        return isPremium;
    }

    @JsonProperty("isPremium")
    public void setIsPremium(Boolean isPremium) {
        this.isPremium = isPremium;
    }



    @Override
    public int compareTo(ScreenerFieldDefinition o) {
        return this.displayName.compareTo(o.displayName);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof ScreenerFieldDefinition)) return false;
        ScreenerFieldDefinition that = (ScreenerFieldDefinition) o;
        return Objects.equals(fieldId, that.fieldId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(fieldId);
    }
}
