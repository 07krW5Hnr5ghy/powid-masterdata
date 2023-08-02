package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.MembershipType;
import com.proyect.masterdata.dto.MasterListDTO;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface MembershipTypeMapper {
    MembershipTypeMapper INSTANCE = Mappers.getMapper(MembershipTypeMapper.class);
    @Mapping(source="id",target = "id")
    @Mapping(source="name",target = "name")
    @Mapping(source = "status",target = "status")
    MasterListDTO membershipTypeToMembershipTypeDTO(MembershipType membershipType);
    List<MasterListDTO> membershipTypeListToMembershipTypeListDTO(List<MembershipType> membershipTypes);
}
