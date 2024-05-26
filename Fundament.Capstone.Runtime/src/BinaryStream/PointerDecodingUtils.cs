namespace Fundament.Capstone.Runtime.BinaryStream;

using CommunityToolkit.Diagnostics;

using Fundament.Capstone.Runtime.Exceptions;

public static class PointerDecodingUtils
{
    /// <summary>
    /// Decodes the struct pointer at the given index in the segment.
    /// This method checks that the word is a struct pointer, and that the offset is within the bounds of the segment.
    /// </summary>
    /// <param name="segment">Segment to get the pointer from.</param>
    /// <param name="index">Index of the pointer in the segment.</param>
    /// <returns>The data encoded in the word as a <see cref="StructPointer">.</returns>
    /// <exception cref="ArgumentOutOfRangeException">If the index is out of bounds for segment.</exception>
    /// <exception cref="TypeTagMismatchException">If the tag of the word does not match the expected tag.</exception>
    /// <exception cref="PointerOffsetOutOfRangeException">If the offset points outside of the bounds of the segment.</exception>
    public static StructPointer DecodeStructPointer(ReadOnlySpan<Word> segment, int index)
    {
        var word = GetTaggedWord(segment, index, PointerType.Struct);

        // First 30 bits after the tag are the offset, as a signed integer
        var offset = int.CreateChecked(word >> 2 & Bits.BitMaskOf(30));
        CheckPointerOffset(segment, index, offset);
        // Next 16 bits are the size of the data section
        var dataSize = ushort.CreateChecked(word >> 32 & Bits.BitMaskOf(16));
        // Last 16 bits are the size of the pointer section
        var pointerSize = ushort.CreateChecked(word >> 48 & Bits.BitMaskOf(16));

        return new StructPointer(offset, dataSize, pointerSize);
    }

    /// <summary>
    /// Decodes the list pointer at the given index in the segment.
    /// This method checks that the word is a struct pointer, and that the offset is within the bounds of the segment.
    /// </summary>
    /// <param name="segment">Segment to get the pointer from.</param>
    /// <param name="index">Index of the pointer in the segment.</param>
    /// <returns>The data encoded in the word as a <see cref="ListPointer"/>.</returns>
    /// <exception cref="ArgumentOutOfRangeException">If the index is out of bounds for segment.</exception>
    /// <exception cref="TypeTagMismatchException">If the tag of the word does not match the expected tag.</exception>
    /// <exception cref="PointerOffsetOutOfRangeException">If the offset points outside of the bounds of the segment.</exception>
    public static ListPointer DecodeListPointer(ReadOnlySpan<Word> segment, int index)
    {
        var word = GetTaggedWord(segment, index, PointerType.List);

        // First 30 bits after the tag are the offset, as a signed integer
        var offset = int.CreateChecked(word >> 2 & Bits.BitMaskOf(30));
        CheckPointerOffset(segment, index, offset);
        // Next 3 bits are the element size
        var elementSize = CreateCheckedListElementType(word >> 32 & Bits.BitMaskOf(3));
        // Last 29 bits represent the size of the list
        var size = uint.CreateChecked(word >> 35 & Bits.BitMaskOf(29));

        return new ListPointer(offset, elementSize, size);
    }

    /// <summary>
    /// Decodes a far pointer from a segment.
    /// The caller must validate the segment id and offset, as this method is unable to check bounds outside of the provided segment.
    /// </summary>
    /// <param name="segment">Segment to get the pointer from.</param>
    /// <param name="index">Index of the pointer in the segment.</param>
    /// <returns>The data encoded in the word as a <see cref="FarPointer"/>.</returns>
    /// <exception cref="ArgumentOutOfRangeException">If the index is out of bounds for segment.</exception>
    /// <exception cref="TypeTagMismatchException">If the tag of the word does not match the expected tag.</exception>
    public static FarPointer DecodeFarPointer(ReadOnlySpan<Word> segment, int index)
    {
        var word = GetTaggedWord(segment, index, PointerType.Far);

        return new FarPointer(
            // First bit is the double-far flag
            (word >> 2 & 1) == 1,
            // Next 29 bits are the offset. This is offset in words from the start of the target segment, so we can't do bounds checking here.
            uint.CreateChecked(word >> 3 & Bits.BitMaskOf(29)),
            // Last 32 bits are the segment id.
            uint.CreateChecked(word >> 32 & Bits.BitMaskOf(32))
        );
    }

    /// <summary>
    /// Decodes a capability pointer from a segment.
    /// </summary>
    /// <param name="segment">The segment to get the pointer from.</param>
    /// <param name="index">The index of the pointer in the segment.</param>
    /// <returns>The data encoded in the word as a <see cref="CapabilityPointer"/>.</returns>
    /// <exception cref="ArgumentOutOfRangeException">If the index is out of bounds for segment.</exception>
    /// <exception cref="TypeTagMismatchException">If the tag of the word does not match the expected tag.</exception>
    public static CapabilityPointer DecodeCapabilityPointer(ReadOnlySpan<Word> segment, int index)
    {
        var word = GetTaggedWord(segment, index, PointerType.Capability);

        // We only care about the last 32 bits of the word, which is the index to the capability table.
        var capabilityOffset = int.CreateChecked(word >> 32 & Bits.BitMaskOf(32));
        CheckPointerOffset(segment, index, capabilityOffset);

        return new CapabilityPointer(capabilityOffset);
    }

    /// <summary>
    /// Checks if the offset of a pointer is within the bounds of the segment.
    /// </summary>
    /// <param name="segment">The segment the pointer resides in.</param>
    /// <param name="wordIndex">The index of the pointer word in the segment.</param>
    /// <param name="offset">The decoded offset of the pointer word.</param>
    /// <exception cref="ArgumentOutOfRangeException">If the word index is out of bounds for segment.</exception>
    /// <exception cref="TypeTagMismatchException">If the tag of the word does not match the expected tag.</exception>
    /// <exception cref="PointerOffsetOutOfRangeException">If the offset points outside of the bounds of the segment.</exception>
    public static void CheckPointerOffset(ReadOnlySpan<Word> segment, int wordIndex, int offset)
    {
        Guard.IsInRangeFor(wordIndex, segment);

        var targetOffset = wordIndex + offset + 1;

        if (targetOffset < 0 || targetOffset <= segment.Length)
        {
            throw new PointerOffsetOutOfRangeException(segment[wordIndex], wordIndex, targetOffset);
        }
    }

    /// <summary>
    /// Helper method that validates the provided value is a valid ListElementType and converts it to the enum.
    /// </summary>
    /// <exception cref="ArgumentOutOfRangeException">If the value is out of range for ListElementType.</exception>
    private static ListElementType CreateCheckedListElementType(Word value)
    {
        Guard.IsBetweenOrEqualTo(value, (byte) ListElementType.Void, (byte) ListElementType.Composite);
        return (ListElementType) value;
    }

    /// <summary>
    /// Helper method to get a word from a segment, performing bounds checking and tag validation.
    /// </summary>
    /// <exception cref="ArgumentOutOfRangeException">If the index is out of bounds for segment.</exception>
    /// <exception cref="TypeTagMismatchException">If the tag of the word does not match the expected tag.</exception>
    private static Word GetTaggedWord(ReadOnlySpan<Word> segment, int index, PointerType type)
    {
        Guard.IsInRangeFor(index, segment);
        var word = segment[index];
        var tag = word & 3;
        var expectedTag = (byte) type;

        return tag != expectedTag
            ? throw new TypeTagMismatchException(word, index, expectedTag)
            : word;
    }
}