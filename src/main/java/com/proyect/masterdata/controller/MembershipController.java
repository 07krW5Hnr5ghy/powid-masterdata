package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IMembership;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/membership")
@AllArgsConstructor
public class MembershipController {
    private final IMembership iMembership;
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("id") Long id,
            @RequestParam("module") String module,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iMembership.save(id,module,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/memberships",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestParam("id") Long id,
            @RequestBody() List<String> modules,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iMembership.saveAll(id,modules,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

}
