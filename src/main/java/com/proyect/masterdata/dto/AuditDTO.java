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
public class AuditDTO {
    private String eventName;
    private String userName;
    private String detail;
    private OffsetDateTime registrationDate;
    private String clientName;
}
