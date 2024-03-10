package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.CourierDTO;
import com.proyect.masterdata.dto.request.RequestCourier;
import com.proyect.masterdata.dto.request.RequestCourierOrder;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ICourier;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@RestController
@CrossOrigin({"*"})
@RequestMapping("courier")
@AllArgsConstructor
public class CourierController {

    private final ICourier iCourier;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:COURIER_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody()RequestCourier requestCourier,
            @RequestParam("tokenUser") String tokenUser
            ) throws BadRequestExceptions{
        ResponseSuccess result = iCourier.save(requestCourier,tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping(value = "order",consumes = MediaType.APPLICATION_JSON_VALUE)
    @PreAuthorize("hasAuthority('ROLE:COURIER') and hasAuthority('ACCESS:COURIER_PUT')")
    public ResponseEntity<ResponseSuccess> updateOrder(
            @RequestParam("orderId") Long orderId,
            @RequestBody() RequestCourierOrder requestCourierOrder,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions{
        ResponseSuccess result = iCourier.updateOrder(orderId,requestCourierOrder,tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    @PreAuthorize("hasAnyAuthority('ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:COURIER_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions{
        ResponseDelete result = iCourier.delete(name,tokenUser);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @GetMapping()
    @PreAuthorize("hasAnyAuthority('ROLE:BUSINESS','ROLE:ADMINISTRATION','ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:COURIER_GET')")
    public ResponseEntity<Page<CourierDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user") String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber") Integer pageNumber,
            @RequestParam(value = "pageSize") Integer pageSize
    ) throws BadRequestExceptions {
        Page<CourierDTO> result = iCourier.list(name,user,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @GetMapping(value = "list-false")
    @PreAuthorize("hasAnyAuthority('ROLE:BUSINESS','ROLE:ADMINISTRATION','ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:COURIER_GET')")
    public ResponseEntity<Page<CourierDTO>> listFalse(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user") String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber") Integer pageNumber,
            @RequestParam(value = "pageSize") Integer pageSize
    ) throws BadRequestExceptions {
        Page<CourierDTO> result = iCourier.listFalse(name,user,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

}
