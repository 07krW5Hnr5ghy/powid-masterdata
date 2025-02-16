package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.ColorDTO;
import com.proyect.masterdata.dto.request.RequestColor;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IColor {
        CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
        CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
        CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
        CompletableFuture<List<ColorDTO>> listColor() throws BadRequestExceptions;
        CompletableFuture<Page<ColorDTO>> list(
                String name,
                OffsetDateTime registrationStartDate,
                OffsetDateTime registrationEndDate,
                OffsetDateTime updateStartDate,
                OffsetDateTime updateEndDate,
                String sort,
                String sortColumn,
                Integer pageNumber,
                Integer pageSize) throws BadRequestExceptions;
        CompletableFuture<Page<ColorDTO>> listStatusFalse(
                String name,
                OffsetDateTime registrationStartDate,
                OffsetDateTime registrationEndDate,
                OffsetDateTime updateStartDate,
                OffsetDateTime updateEndDate,
                String sort,
                String sortColumn,
                Integer pageNumber,
                Integer pageSize) throws BadRequestExceptions;
        CompletableFuture<List<ColorDTO>> listFilter() throws BadRequestExceptions;
}
