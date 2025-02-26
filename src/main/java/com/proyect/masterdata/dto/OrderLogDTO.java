package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class OrderLogDTO {
    private OffsetDateTime registrationDate;
    private String orderState;
    private String userFullName;
}
