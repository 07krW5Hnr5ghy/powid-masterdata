package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.CourierDTO;
import com.proyect.masterdata.dto.request.RequestCourier;
import com.proyect.masterdata.dto.request.RequestCourierOrder;
import com.proyect.masterdata.dto.request.RequestCourierUser;
import com.proyect.masterdata.dto.response.ResponseCourierInfo;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ICourier;
import com.proyect.masterdata.services.IUtil;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({"*"})
@RequestMapping("courier")
@AllArgsConstructor
public class CourierController {
    private final ICourier iCourier;
    private final IUtil iUtil;
    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:COURIER_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody()RequestCourier requestCourier,
            @RequestParam("tokenUser") String tokenUser
            ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iCourier.save(requestCourier,tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @PostMapping("user-to-courier")
    public ResponseEntity<ResponseSuccess> saveToCourier(
            @RequestBody()RequestCourierUser requestCourierUser,
            @RequestParam("tokenUser") String tokenUser
            )throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iCourier.saveCourierToUser(requestCourierUser,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @PutMapping(value = "order")
    //@PreAuthorize("hasAuthority('ROLE:COURIER') and hasAuthority('ACCESS:COURIER_PUT')")
    public ResponseEntity<ResponseSuccess> updateOrder(
            @RequestParam("orderId") UUID orderId,
            @RequestBody() RequestCourierOrder requestCourierOrder,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iCourier.updateOrder(orderId,requestCourierOrder,tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @DeleteMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:COURIER_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("dni") String dni,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iCourier.delete(dni,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:BUSINESS','ROLE:ADMINISTRATION','ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:COURIER_GET')")
    public ResponseEntity<Page<CourierDTO>> list(
            @RequestParam(value = "user") String user,
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "dni",required = false) String dni,
            @RequestParam(value = "company", required = false) String company,
            @RequestParam(value = "registrationStartDate",required = false) String rStartDate,
            @RequestParam(value = "registrationEndDate",required = false) String rEndDate,
            @RequestParam(value = "updateStartDate",required = false) String uStartDate,
            @RequestParam(value = "updateEndDate",required = false) String uEndDate,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber") Integer pageNumber,
            @RequestParam(value = "pageSize") Integer pageSize,
            @RequestParam(value = "status",required = false) Boolean status
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        OffsetDateTime registrationStartDate = iUtil.parseToOffsetDateTime(rStartDate,true);
        OffsetDateTime registrationEndDate = iUtil.parseToOffsetDateTime(rEndDate, false);
        OffsetDateTime updateStartDate = iUtil.parseToOffsetDateTime(uStartDate,true);
        OffsetDateTime updateEndDate = iUtil.parseToOffsetDateTime(uEndDate,false);
        CompletableFuture<Page<CourierDTO>> result = iCourier.list(
                user,
                name,
                dni,
                company,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                sort,
                sortColumn,
                pageNumber,
                pageSize,
                status);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:BUSINESS','ROLE:ADMINISTRATION','ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:COURIER_GET')")
    public ResponseEntity<List<CourierDTO>> listCourier(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<CourierDTO>> result = iCourier.listCouriers(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:BUSINESS','ROLE:ADMINISTRATION','ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:COURIER_GET')")
    public ResponseEntity<List<CourierDTO>> listCourierFalse(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<CourierDTO>> result = iCourier.listCouriersFalse(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @PostMapping("activate")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("dni") String dni,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iCourier.activate(dni, tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("filter")
    //@PreAuthorize("hasAnyAuthority('ROLE:BUSINESS','ROLE:ADMINISTRATION','ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:COURIER_GET')")
    public ResponseEntity<List<CourierDTO>> listFilter(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<CourierDTO>> result = iCourier.listFilters(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("info")
    public ResponseEntity<ResponseCourierInfo> courierInfo(
            @RequestParam("user")String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseCourierInfo> result = iCourier.infoCourier(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

}
