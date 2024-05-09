package com.proyect.masterdata.services;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.PlanDTO;
import com.proyect.masterdata.dto.SubscriptionDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface ISubscription {
    ResponseSuccess save(String name, Integer months, Double discountPercent, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(String name, Integer months, Double discountPercent, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<Page<SubscriptionDTO>> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<List<PlanDTO>> listPlans() throws InternalErrorExceptions;
}
