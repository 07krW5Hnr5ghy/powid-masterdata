package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.CourierDTO;
import com.proyect.masterdata.dto.request.RequestCourier;
import com.proyect.masterdata.dto.request.RequestCourierOrder;
import com.proyect.masterdata.dto.request.RequestCourierUser;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface ICourier {
    CompletableFuture<ResponseSuccess> save(RequestCourier requestCourier, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> saveCourierToUser(RequestCourierUser requestCourierUser, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseDelete> delete(String dni,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<ResponseSuccess> activate(String dni,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<Page<CourierDTO>> list(
            String user,
            String name,
            String dni,
            String company,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) throws BadRequestExceptions;
    CompletableFuture<ResponseSuccess> updateOrder(UUID orderId, RequestCourierOrder requestCourierOrder, String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<List<CourierDTO>> listCouriers(String user) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<CourierDTO>> listCouriersFalse(String user) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<CourierDTO>> listFilters(String user) throws BadRequestExceptions,InternalErrorExceptions;
}
