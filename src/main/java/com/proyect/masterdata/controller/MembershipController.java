package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.MembershipDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IMembership;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
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
            @RequestParam("channel") String channel,
            @RequestParam("module") String module,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iMembership.save(channel,module,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/memberships",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestParam("channel") String channel,
            @RequestBody() List<String> modules,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iMembership.saveAll(channel,modules,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Page<MembershipDTO>> list(
            @RequestParam(value = "channel",required = false) String channel,
            @RequestParam(value = "module",required = false) String module,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions {
        Page<MembershipDTO> result = iMembership.list(channel,module,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

}
