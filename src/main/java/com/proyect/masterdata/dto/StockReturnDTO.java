package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.Date;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class StockReturnDTO {
    private String serial;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
    private String supplier;
    private String warehouse;
}
