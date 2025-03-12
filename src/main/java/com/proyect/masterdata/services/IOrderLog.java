package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.OrderLog;
import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.OrderLogDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IOrderLog {
    OrderLog save(User user, Ordering order, String detail) throws InternalErrorExceptions, BadRequestExceptions;
    List<OrderLogDTO> listLogByOrder(UUID orderId) throws InternalErrorExceptions,BadRequestExceptions;
}
