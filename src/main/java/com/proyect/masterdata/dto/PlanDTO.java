package com.proyect.masterdata.dto;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class PlanDTO {
    private String name;
    private Integer months;
    private Double discountPercentaje;
    private List<ModulePlanDTO> moduleList;
}
