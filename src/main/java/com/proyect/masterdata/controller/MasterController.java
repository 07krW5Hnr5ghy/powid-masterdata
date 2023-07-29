package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.PaymentMethodDTO;
import com.proyect.masterdata.dto.PaymentStateDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.handler.ErrorResponse;
import com.proyect.masterdata.services.IDepartment;
import com.proyect.masterdata.services.IPaymentMethod;
import com.proyect.masterdata.services.IPaymentState;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/")
@AllArgsConstructor
public class MasterController {

}
