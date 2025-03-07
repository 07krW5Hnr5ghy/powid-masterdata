package com.proyect.masterdata.dto;

import java.util.List;
import java.util.UUID;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class PlanDTO {
    private UUID id;
    private String name;
    private Integer months;
    private Double discountPercentage;
    private List<ModulePlanDTO> moduleList;
    private Boolean status;
    private String user;
}
