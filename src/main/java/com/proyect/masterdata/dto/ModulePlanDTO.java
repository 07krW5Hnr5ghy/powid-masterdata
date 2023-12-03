package com.proyect.masterdata.dto;

import java.math.BigDecimal;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ModulePlanDTO {
    String moduleName;
    BigDecimal modulePrice;
}
