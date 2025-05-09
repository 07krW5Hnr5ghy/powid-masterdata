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
    private String paymentMethod;
    private String connection;
    private String user;
}
