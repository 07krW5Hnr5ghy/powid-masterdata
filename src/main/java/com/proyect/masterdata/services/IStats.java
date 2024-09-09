package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.StatsCardDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IStats {
    CompletableFuture<StatsCardDTO> listCardStats(
            Date updateStartDate,
            Date updateEndDate,
            String orderState,
            String user) throws BadRequestExceptions,InterruptedException;
}
