package com.proyect.masterdata.controller;

import com.proyect.masterdata.domain.MembershipType;
import com.proyect.masterdata.dto.LogEventDTO;
import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.MembershipTypeDTO;
import com.proyect.masterdata.dto.request.RequestLogEvent;
import com.proyect.masterdata.dto.request.RequestMasterList;
import com.proyect.masterdata.dto.request.RequestMembershipType;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.handler.ErrorResponse;
import com.proyect.masterdata.services.IMembershipType;
import com.proyect.masterdata.services.impl.MembershipTypeImpl;
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
@RequestMapping("/membership-type")
@AllArgsConstructor
public class MembershipTypeController {
    private final IMembershipType iMembershipType;

    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name
    ) throws BadRequestExceptions {
        ResponseSuccess result = iMembershipType.save(name);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/departments")
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestBody() List<String> names
    ) throws BadRequestExceptions {
        ResponseSuccess result = iMembershipType.saveAll(names);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping()
    public ResponseEntity<MembershipTypeDTO> update(
            @RequestBody() RequestMembershipType requestMembershipType
    ) throws BadRequestExceptions {
        MembershipTypeDTO result = iMembershipType.update(requestMembershipType);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        ResponseDelete result = iMembershipType.delete(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(value = "/departments")
    public ResponseEntity<ResponseDelete> deleteall(
            @RequestBody() List<Long> codes
    ) throws BadRequestExceptions {
        ResponseDelete result = iMembershipType.deleteAll(codes);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<List<MembershipTypeDTO>> list() throws BadRequestExceptions {
        List<MembershipTypeDTO> result = iMembershipType.list();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code")
    public ResponseEntity<MembershipTypeDTO> findByCode(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        MembershipTypeDTO result = iMembershipType.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/name")
    public ResponseEntity<MembershipTypeDTO> findByName(
            @RequestParam("name") String name
    ) throws BadRequestExceptions {
        MembershipTypeDTO result = iMembershipType.findByName(name);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
