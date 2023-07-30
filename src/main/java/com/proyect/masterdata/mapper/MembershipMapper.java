package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.Membership;
import com.proyect.masterdata.dto.MasterListDTO;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface MembershipMapper {
    MembershipMapper INSTANCE = Mappers.getMapper(MembershipMapper.class);
    @Mapping(source="id",target = "id")
    @Mapping(source="name",target = "name")
    @Mapping(source = "status",target = "status")
    MasterListDTO membershipToMembershipDTO(Membership membership);
    List<MasterListDTO> membershipListToMembershipListDTO(List<Membership> memberships);
}
