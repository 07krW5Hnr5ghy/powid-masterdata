package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class RequestChannelSave {
    private String name;
    private Integer months;
    private String client;
    private Long membership;
    private String paymentType;
    private String connection;
    private String user;
}
