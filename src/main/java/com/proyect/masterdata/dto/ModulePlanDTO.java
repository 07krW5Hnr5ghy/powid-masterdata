package com.proyect.masterdata.dto;

import java.math.BigDecimal;
import java.util.UUID;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ModulePlanDTO {
    private String moduleName;
    private BigDecimal modulePrice;
    private UUID id;
    private String user;
    private Boolean status;
}
